cargo build --release --bin json_parse
set "DTRACE=./blondie_dtrace.exe" &  cargo flamegraph --bin json_parse
:: ; ./flamegraph.svg