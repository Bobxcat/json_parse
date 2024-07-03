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
                || black_box(BufReader::new(File::open(file).unwrap())),
                |f| b.parse_stream(f),
                BatchSize::SmallInput,
            )
        });
    }
    let s = fs::read_to_string(file).unwrap();
    grp.bench_function(format!("{}_from_memory", b.name().into()), |bencher| {
        bencher.iter(|| b.parse_str(black_box(&s)))
    });
}

trait Bench {
    const CAN_STREAM: bool;

    fn name(&self) -> impl Into<String>;
    fn parse_str(&self, s: &str);
    fn parse_stream(&self, s: BufReader<impl Read>);
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

    fn parse_stream(&self, s: BufReader<impl Read>) {
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

    fn parse_stream(&self, _: BufReader<impl Read>) {
        unimplemented!()
    }
}

/// First API, no error handling
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

            fn parse_stream(&self, s: BufReader<impl Read>) {
                let mut p = $root::parse::JsonParser::new(s);
                let x = p.parse();
                black_box((x, p));
            }
        }
    };
}

bench_json_parse_api0!(BenchJsonParse0, parse_0, "json_parse_0");
bench_json_parse_api0!(BenchJsonParse1, parse_1, "json_parse_1");
bench_json_parse_api0!(BenchJsonParse2, parse_2, "json_parse_2");
bench_json_parse_api0!(BenchJsonParse3, parse_3, "json_parse_3");

/// Second API, error handling (unwrap errors)
macro_rules! bench_json_parse_api1 {
    ($struct_name:ident, $root:ident, $bench_name:literal) => {
        struct $struct_name;

        impl Bench for $struct_name {
            const CAN_STREAM: bool = true;

            fn name(&self) -> impl Into<String> {
                $bench_name
            }

            fn parse_str(&self, s: &str) {
                let mut p = $root::parse::JsonParser::new(s.as_bytes());
                let x = p.parse().unwrap();
                black_box((x, p));
            }

            fn parse_stream(&self, s: BufReader<impl Read>) {
                let mut p = $root::parse::JsonParser::new(s);
                let x = p.parse().unwrap();
                black_box((x, p));
            }
        }
    };
}

bench_json_parse_api1!(
    BenchJsonParse3ErrHandling,
    parse_3_err_handling,
    "json_parse_3_err_handling"
);
bench_json_parse_api1!(
    BenchJsonParse4CustomIntern,
    parse_4_custom_intern,
    "json_parse_4_custom_intern"
);
bench_json_parse_api1!(
    BenchJsonParse4PreallocString,
    parse_4_prealloc_string,
    "json_parse_4_prealloc_string"
);
bench_json_parse_api1!(BenchJsonParseCurr, json_parse, "json_parse_curr");

fn do_bench_all(grp: &mut BenchmarkGroup<WallTime>, path: PathBuf) {
    do_bench(grp, &path, BenchSerdeJson);
    do_bench(grp, &path, BenchJson);
    do_bench(grp, &path, BenchJsonParse0);
    do_bench(grp, &path, BenchJsonParse1);
    do_bench(grp, &path, BenchJsonParse2);
    do_bench(grp, &path, BenchJsonParse3);
    do_bench(grp, &path, BenchJsonParse3ErrHandling);
    do_bench(grp, &path, BenchJsonParse4CustomIntern);
    do_bench(grp, &path, BenchJsonParse4PreallocString);
    do_bench(grp, &path, BenchJsonParseCurr);
}

pub fn entry(c: &mut Criterion) {
    let path = black_box(PathBuf::from("./twitter.json"));
    let grp = &mut c.benchmark_group("Parse Twitter File");
    do_bench_all(grp, path);
}

criterion_group!(benches, entry);
criterion_main!(benches);
