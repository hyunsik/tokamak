use std::cell::Cell;
use std::iter::repeat;
use std::time::{Duration, Instant};

pub fn time<T, F>(do_it: bool, what: &str, f: F) -> T where
    F: FnOnce() -> T,
{
    thread_local!(static DEPTH: Cell<usize> = Cell::new(0));
    if !do_it { return f(); }

    let old = DEPTH.with(|slot| {
        let r = slot.get();
        slot.set(r + 1);
        r
    });

    let start = Instant::now();
    let rv = f();
    let dur = start.elapsed();

    let mem_string = match get_resident() {
        Some(n) => {
            let mb = n as f64 / 1_000_000.0;
            format!("; rss: {}MB", mb.round() as usize)
        }
        None => "".to_owned(),
    };
    println!("{}time: {}{}\t{}",
             repeat("  ").take(old).collect::<String>(),
             duration_to_secs_str(dur),
             mem_string,
             what);

    DEPTH.with(|slot| slot.set(old));

    rv
}

// Hack up our own formatting for the duration to make it easier for scripts
// to parse (always use the same number of decimal places and the same unit).
pub fn duration_to_secs_str(dur: Duration) -> String {
    const NANOS_PER_SEC: f64 = 1_000_000_000.0;
    let secs = dur.as_secs() as f64 +
               dur.subsec_nanos() as f64 / NANOS_PER_SEC;

    format!("{:.3}", secs)
}

pub fn to_readable_str(mut val: usize) -> String {
    let mut groups = vec![];
    loop {
        let group = val % 1000;

        val /= 1000;

        if val == 0 {
            groups.push(format!("{}", group));
            break
        } else {
            groups.push(format!("{:03}", group));
        }
    }

    groups.reverse();

    groups.join("_")
}

pub fn record_time<T, F>(accu: &Cell<Duration>, f: F) -> T where
    F: FnOnce() -> T,
{
    let start = Instant::now();
    let rv = f();
    let duration = start.elapsed();
    accu.set(duration + accu.get());
    rv
}

// Like std::macros::try!, but for Option<>.
macro_rules! option_try(
    ($e:expr) => (match $e { Some(e) => e, None => return None })
);

#[cfg(unix)]
fn get_resident() -> Option<usize> {
    use std::fs::File;
    use std::io::Read;

    let field = 1;
    let mut f = option_try!(File::open("/proc/self/statm").ok());
    let mut contents = String::new();
    option_try!(f.read_to_string(&mut contents).ok());
    let s = option_try!(contents.split_whitespace().nth(field));
    let npages = option_try!(s.parse::<usize>().ok());
    Some(npages * 4096)
}

#[test]
fn test_time() {
  time(true, "test1", || 1 + 1);
}