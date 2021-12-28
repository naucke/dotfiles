use reqwest::blocking::Client;
use std::{env, thread, time};

enum Metric {
    Load,
    Temperature,
    Memory,
}

const URL: &str = "http://localhost:9101/metrics/";
const ERROR: &str = "Specify one of: load, temperature, memory";

const MAX_TEMP: i8 = 80;
const TEMPS: &[&str] = &["", "", "", "", ""];

fn main() {
    let metric = parse_metric();
    let client = Client::builder()
        .timeout(time::Duration::from_millis(500))
        .build()
        .unwrap();
    let second = time::Duration::from_secs(1);
    loop {
        println!(
            "{}",
            match client.get(URL).send() {
                Ok(res) => retrieve_metric(&metric, res.text().unwrap()),
                Err(_) => String::from(""),
            }
        );
        thread::sleep(second);
    }
}

fn parse_metric() -> Metric {
    let args: Vec<String> = env::args().collect();
    assert!(args.len() == 2, "{}", ERROR);
    match args[1].as_str() {
        "load" => Metric::Load,
        "temperature" => Metric::Temperature,
        "memory" => Metric::Memory,
        _ => panic!("{}", ERROR),
    }
}

fn retrieve_metric(metric: &Metric, body: String) -> String {
    let metric_string = match metric {
        Metric::Load => "utilization_gpu",
        Metric::Temperature => "temperature_gpu",
        Metric::Memory => "utilization_memory",
    };

    let raw_number = body
        .lines()
        .find(|s| s.starts_with(metric_string))
        .unwrap()
        .rsplit_once(' ')
        .unwrap()
        .1;

    match metric {
        Metric::Load => format!("{:02}% ", raw_number),
        Metric::Temperature => {
            let temp = raw_number.parse::<i8>().unwrap();
            let red_span = if temp >= MAX_TEMP {
                ("<span color=\"#ff0000\">", "</span>")
            } else {
                ("", "")
            };
            format!(
                "{}{}°C {}{}",
                red_span.0,
                temp,
                get_thermometer(temp),
                red_span.1
            )
        }
        Metric::Memory => format!("{:02}% ", raw_number),
    }
}

fn get_thermometer(temp: i8) -> &'static str {
    let mut idx = (((TEMPS.len() - 1) as f32 / MAX_TEMP as f32) * temp as f32).floor() as i8;
    idx = if idx < 0 {
        0
    } else if idx as usize >= TEMPS.len() {
        TEMPS.len() as i8 - 1
    } else {
        idx
    };
    TEMPS[idx as usize]
}
