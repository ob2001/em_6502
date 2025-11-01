use clap::Parser;
use em_6502::{cpu::CPU6502, mem::Mem};
// use em_6502::{cpu::CPU6502, mem::Mem};

fn main() {
    let args = Cli::parse();

    let mut cpu = CPU6502::new();
    if args.mem_init_mode != None {
        cpu = if let Ok(byte)  = u8::from_str_radix(args.mem_init_mode.clone().unwrap().as_str(), 16) {
            CPU6502::new_with_mem(Mem::new_all(byte))
        } else if let Some(mode) = args.mem_init_mode {
            match mode.as_str() {
                "nops" => CPU6502::new_with_mem(Mem::new_nops()),
                "hlts" => CPU6502::new_with_mem(Mem::new_hlts()),
                "file" => {
                    match args.in_file {
                        Some(file) => CPU6502::new_with_mem_from_file(file).unwrap(),
                        None => panic!("Please use the \"-f\" option to specify the memory specification file"),
                    }
                }
                _ => CPU6502::new()
            }
        } else {
            CPU6502::new()
        };
    }

    cpu.set_allow_hlt(args.allow_hlt.unwrap_or(true));
    cpu.set_cycle_limit(args.cycle_limit.unwrap_or(0));
    cpu.set_illegal_opcode_mode(args.illegal_opcode_mode.unwrap_or(true));

    cpu.power_on_and_run(args.debug_mode.unwrap_or(true));

    // Print CPU and memory postmortem
    if args.postmortem.unwrap_or(false) {
        println!("vvv CPU and Memory Postmortem vvv");
        println!("{:?}", cpu);
    }
}

#[derive(Parser)]
struct Cli {
    #[arg(short = 'm', long = "mem_init_mode")]
    mem_init_mode: Option<String>,
    #[arg(short = 'f', long = "in_file", value_name = "FILE")]
    in_file: Option<String>,
    #[arg(short = 'd', long = "debug_mode")]
    debug_mode: Option<bool>,
    #[arg(short = 'l', long = "allow_hlt")]
    allow_hlt: Option<bool>,
    #[arg(short = 'i', long = "ill_op_mode")]
    illegal_opcode_mode: Option<bool>,
    #[arg(short = 'c', long = "cycle_limit")]
    cycle_limit: Option<usize>,
    #[arg(short = 'p', long = "postmortem")]
    postmortem: Option<bool>,
}
