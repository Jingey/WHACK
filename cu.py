from enum import Enum

from wires import Wire, Bus, Register


class Opcode(Enum):
    HLT = 0
    ADD = 1
    XOR = 2
    AND = 3
    NOT = 4
    LDA = 5
    JMP = 6
    INP = 7
    OUT = 8


class Cu:

    def __init__(
            self, enable: Wire, cu_bus: Bus, cu_bus_output: Wire, func_bus: Bus, alu_enable: Wire,
            acc_in_select: Wire, acc_out_select: Wire, acc_read: Wire, acc_write: Wire, r1: Register, r2: Register,
            mar_enable: Wire, main_store_enable: Wire, rw_bus: Bus, cir_read: Wire, cir_write: Wire, pc_read: Wire, pc_write: Wire, pc_increment: Wire,
            read_input: Wire, write_output: Wire, reset_cycle: Register, halt: Wire
    ):
        self.enable = enable
        self.cu_bus = cu_bus
        self.cu_bus_output = cu_bus_output
        self.func_bus = func_bus
        self.alu_enable = alu_enable
        self.acc_in_select = acc_in_select
        self.acc_out_select = acc_out_select
        self.acc_read = acc_read
        self.acc_write = acc_write

        # General purpose registers
        self.r1 = r1
        self.r2 = r2

        self.mar_enable = mar_enable
        self.main_store_enable = main_store_enable
        self.rw_bus = rw_bus

        self.cir_read = cir_read
        self.cir_write = cir_write

        self.pc_read = pc_read
        self.pc_write = pc_write
        self.pc_increment = pc_increment

        self.read_input = read_input
        self.write_output = write_output

        self.reset_cycle = reset_cycle
        self.halt = halt

    def load_next_instruction(self):
        # Send pc to mar.
        self.mar_enable.enlist(self.pc_write.enable())

        # Increment pc
        self.pc_increment.enable()

    def fetch(self):
        pass

    def execute(self):
        pass
