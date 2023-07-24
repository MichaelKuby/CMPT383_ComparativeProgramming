use std::sync::mpsc;
use std::sync::mpsc::RecvError;
use std::thread;
use spmc::Sender;

pub trait Task {
    type Output: Send;
    /* run may return either a value of type Output wrapped in a Some variant, or a None variant
    to indicate absence.*/
    fn run(&self) -> Option<Self::Output>;
}

/*
*************************************************************************************************
Represents a work queue that can be used to distribute tasks among multiple worker threads and
collect their output.
*************************************************************************************************

Struct that takes a generic type parameter TaskType. TaskType represents the type of tasks that
can be enqueued and processed. TaskType must implement Task and Send, and be 'static.
'static represents the lifetime of the entire program. Is created at compile time.
 */
pub struct WorkQueue<TaskType: 'static + Task + Send> {
    // Option because it will be set to None to close the queue
    send_tasks: Option<spmc::Sender<TaskType>>,

    // Receive tasks from the work queue.
    recv_tasks: spmc::Receiver<TaskType>,

    // not need in the struct: each worker will have its own clone.
    //send_output: mpsc::Sender<TaskType::Output>,

    // Used to receive the output of completed tasks.
    recv_output: mpsc::Receiver<TaskType::Output>,

    // Stores handles to the worker threads that are processing tasks
    workers: Vec<thread::JoinHandle<()>>,
}

impl<TaskType: 'static + Task + Send> WorkQueue<TaskType> {
    pub fn new(n_workers: usize) -> WorkQueue<TaskType> {
        // create the channels; start the worker threads; record their JoinHandles

        // Create a single producer multiple consumer channel to send jobs to workers
        let (send_task, receive_task) = spmc::channel();

        // Create a multiple producer single consumer channel to receive work done by workers.
        let (send_output, receive_output) = mpsc::channel();

        let mut workers = Vec::with_capacity(n_workers);

        for _ in 0..n_workers {
            let rec = receive_task.clone();
            let snd = send_output.clone();
            let handle = thread::spawn(move || Self::run(rec, snd)
            );
            workers.push(handle);
        }

        WorkQueue {
            send_tasks: Some(send_task),
            recv_tasks: receive_task,
            recv_output: receive_output,
            workers,
        }
    }

    fn run(recv_tasks: spmc::Receiver<TaskType>, send_output: mpsc::Sender<TaskType::Output>) {
        // TODO: the main logic for a worker thread
        loop {
            let task_result = recv_tasks.recv();
            match task_result {
                Ok(task) => {
                    // Do something with the task
                    let possible_task = task.run();
                    match possible_task {
                        None => {return} // Ignore and continue processing
                        Some(t) => {send_output.send(t).unwrap();}
                    }
                },
                Err(_) => {
                    // task_result will be Err() if the spmc::Sender has been destroyed
                    // and no more messages can be received here
                    return
                }
            };
        }
    }

    pub fn enqueue(&mut self, t: TaskType) -> Result<(), spmc::SendError<TaskType>> {
        match &mut self.send_tasks {
            None => Err(spmc::SendError(t)),
            Some(s) => s.send(t),
        }
    }

    // Helper methods that let you receive results in various ways
    pub fn iter(&mut self) -> mpsc::Iter<TaskType::Output> {
        self.recv_output.iter()
    }
    pub fn recv(&mut self) -> TaskType::Output {
        self.recv_output
            .recv()
            .expect("I have been shutdown incorrectly")
    }
    pub fn try_recv(&mut self) -> Result<TaskType::Output, mpsc::TryRecvError> {
        self.recv_output.try_recv()
    }
    pub fn recv_timeout(
        &self,
        timeout: std::time::Duration,
    ) -> Result<TaskType::Output, mpsc::RecvTimeoutError> {
        self.recv_output.recv_timeout(timeout)
    }

    pub fn shutdown(&mut self) {
        // Destroy the spmc::Sender so everybody knows no more tasks are incoming;
        self.send_tasks = None;

        // drain any pending tasks in the queue
        loop {
            let res = self.recv_tasks.recv();
            match res {
                Ok(_) => {} // continue
                Err(_) => {break}
            }
        }

        // Drain self.workers, join and discard
        for handle in self.workers.drain(..) {
            handle.join().unwrap();
        }
    }
}

impl<TaskType: 'static + Task + Send> Drop for WorkQueue<TaskType> {
    fn drop(&mut self) {
        // "Finalisation in destructors" pattern: https://rust-unofficial.github.io/patterns/idioms/dtor-finally.html
        match self.send_tasks {
            None => {} // already shut down
            Some(_) => self.shutdown(),
        }
    }
}
