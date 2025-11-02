use clap::Parser;
use em_6502::{cpu::CPU6502, mem::Mem};

fn main() {
    let args = Cli::parse();
    let mut file_mode = false;

    let mut cpu = if let Some(mode) = args.mem_init_mode {
        if let Ok(byte)  = u8::from_str_radix(&mode, 16) {
            CPU6502::new_with_mem(Mem::new_all(byte))
        } else {
            match mode.as_str() {
                "nops" => CPU6502::new_with_mem(Mem::new_nops()),
                "hlts" => CPU6502::new_with_mem(Mem::new_hlts()),
                "file" => {
                    file_mode = true;
                    match args.in_file {
                        Some(file) => CPU6502::new_with_mem_from_file(file).unwrap(),
                        None => panic!("Please use the \"-f\" option to specify the memory specification file"),
                    }
                }
                _ => { 
                    eprintln!("Error with argument to -m/--mem_init_mode: {mode} is not a valid option.\nDefaulting to a new CPU with default memory.");
                    CPU6502::new()
                }
            }
        }
    } else {
        CPU6502::new()
    };

    cpu.set_allow_hlt(args.allow_hlt);

    if args.cycle_limit != None {
        cpu.set_cycle_limit(args.cycle_limit.unwrap());
    } else if file_mode {
        cpu.set_cycle_limit(0);
    } else {
        cpu.set_cycle_limit(10);
    }

    cpu.set_illegal_opcode_mode(args.illegal_opcode_mode);

    cpu.power_on_and_run(args.debug_mode);

    // Print CPU and memory postmortem
    if args.postmortem {
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

    #[arg(short = 'd', long = "debug_mode", default_value_t = true)]
    debug_mode: bool,

    #[arg(short = 'l', long = "allow_hlt", default_value_t = true)]
    allow_hlt: bool,

    #[arg(short = 'i', long = "ill_op_mode", default_value_t =  true)]
    illegal_opcode_mode: bool,

    #[arg(short = 'c', long = "cycle_limit")]
    cycle_limit: Option<usize>,

    #[arg(short = 'p', long = "postmortem", default_value_t = false)]
    postmortem: bool,
}
