//!
//! Task
//!

use std::path::{Path, PathBuf};
use std::sync::{Mutex};


pub struct TaskAttempt {
  pub id: String,
  pub working_dir: PathBuf,
  pub progress: Mutex<f32>,
  pub stat: TaskStat,  
}

impl TaskAttempt {
  fn new(id: &str, dir: &Path) -> TaskAttempt {
    TaskAttempt {
      id: id.to_string(),
      working_dir: dir.to_path_buf(),
      progress: Mutex::new(0f32),
      stat: TaskStat {start_time: 0f64, end_time: 0f64}
    }
  }

  #[allow(dead_code)]
  fn start() {}

  #[allow(dead_code)]
  fn stop() {}

  #[allow(dead_code)]
  fn kill() {}

  fn progress() -> f32 {
    0f32
  }
}

pub struct TaskStat {
  pub start_time: f64,
  pub end_time: f64,

  // TODO - to be implemented
  //shuffle_start_time: f64,
  //shuffle_end_time: f64,
}

impl TaskStat {
  fn start_time(&mut self, time: f64) {
    self.start_time = time;
  }

  fn end_time(&mut self, time: f64) {
    self.end_time = time;
  }
}