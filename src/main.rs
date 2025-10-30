use em_6502::cpu::CPU6502;

fn main() {
    let mut cpu = CPU6502::new_with_mem_from_file("in_sample.txt".to_string()).unwrap();
    cpu.set_allow_hlt(false);
    cpu.set_illegal_opcode_mode(true);
    cpu.set_cycle_limit(10);

    cpu.power_on_and_run(true);

    // Print CPU and memory postmortem
    if false {
        println!("vvv CPU and Memory Postmortem vvv");
        println!("{:?}", cpu);
    }
}
