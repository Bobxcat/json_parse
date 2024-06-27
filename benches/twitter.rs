use std::{
    fs::{self, File},
    io::{BufReader, Read},
    path::{Path, PathBuf},
};

use criterion::{
    black_box, criterion_group, criterion_main, measurement::WallTime, BatchSize, BenchmarkGroup,
    Criterion,
};

fn do_bench<B: Bench>(grp: &mut BenchmarkGroup<WallTime>, file: &Path, b: B) {
    if B::CAN_STREAM {
        grp.bench_function(b.name().into(), |bencher| {
            bencher.iter_batched(
                || black_box(File::open(file).unwrap()),
                |f| b.parse_stream(f),
                BatchSize::SmallInput,
            )
        });
    }
    let s = fs::read_to_string(file).unwrap();
    grp.bench_function(format!("{}_no_streaming", b.name().into()), |bencher| {
        bencher.iter(|| b.parse_str(black_box(&s)))
    });
}

trait Bench {
    const CAN_STREAM: bool;

    fn name(&self) -> impl Into<String>;
    fn parse_str(&self, s: &str);
    fn parse_stream(&self, s: impl Read);
}

struct BenchSerdeJson;

impl Bench for BenchSerdeJson {
    const CAN_STREAM: bool = true;

    fn name(&self) -> impl Into<String> {
        "serde_json"
    }

    fn parse_str(&self, s: &str) {
        let x: serde_json::Value = serde_json::from_str(s).unwrap();
        black_box(x);
    }

    fn parse_stream(&self, s: impl Read) {
        let x: serde_json::Value = serde_json::from_reader(s).unwrap();
        black_box(x);
    }
}

struct BenchJson;

impl Bench for BenchJson {
    const CAN_STREAM: bool = false;

    fn name(&self) -> impl Into<String> {
        "json"
    }

    fn parse_str(&self, s: &str) {
        black_box(json::parse(s).unwrap());
    }

    fn parse_stream(&self, _: impl Read) {
        unimplemented!()
    }
}

macro_rules! bench_json_parse_api0 {
    ($struct_name:ident, $root:ident, $bench_name:literal) => {
        struct $struct_name;

        impl Bench for $struct_name {
            const CAN_STREAM: bool = true;

            fn name(&self) -> impl Into<String> {
                $bench_name
            }

            fn parse_str(&self, s: &str) {
                let mut p = $root::parse::JsonParser::new(s.as_bytes());
                let x = p.parse();
                black_box((x, p));
            }

            fn parse_stream(&self, s: impl Read) {
                let mut p = $root::parse::JsonParser::new(BufReader::new(s));
                let x = p.parse();
                black_box((x, p));
            }
        }
    };
}

bench_json_parse_api0!(BenchJsonParse1, json_parse, "json_parse_0");

struct BenchJsonParse0;

impl Bench for BenchJsonParse0 {
    const CAN_STREAM: bool = true;

    fn name(&self) -> impl Into<String> {
        "json_parse_0"
    }

    fn parse_str(&self, s: &str) {
        let mut p = json_parse::parse::JsonParser::new(s.as_bytes());
        let x = p.parse();
        black_box((x, p));
    }

    fn parse_stream(&self, s: impl Read) {
        let mut p = json_parse::parse::JsonParser::new(BufReader::new(s));
        let x = p.parse();
        black_box((x, p));
    }
}

pub fn entry(c: &mut Criterion) {
    let path = black_box(PathBuf::from("./twitter.json"));

    let grp = &mut c.benchmark_group("Parse Twitter File");

    do_bench(grp, &path, BenchSerdeJson);
    do_bench(grp, &path, BenchJson);
    do_bench(grp, &path, BenchJsonParse0);
}

criterion_group!(benches, entry);
criterion_main!(benches);
