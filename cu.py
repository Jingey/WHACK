from enum import Enum

from wires import Wire, Bus, Register


class Opcode(Enum):
    NOP = 0b0000
    JMP = 0b0001
    JEZ = 0b0010
    JNV = 0b0011
    STR = 0b0100
    LDR = 0b0101
    MOV = 0b0110
    LS = 0b0111
    ADD = 0b1000
    SUB = 0b1001
    AND = 0b1010
    OR = 0b1011
    NOT = 0b1100
    HLT = 0b1111


class Cu:
    def __init__(
            self, enable: Wire, cu_bus: Bus, cu_bus_output: Wire, func_bus: Bus, alu_enable: Wire,
            acc_in_select: Wire, acc_out_select: Wire, acc_read: Wire, acc_write: Wire, r1: Register, r2: Register,
            mar_enable: Wire, main_store_enable: Wire, rw_bus: Bus, cir_read: Wire, cir_write: Wire, pc_read: Wire,
            pc_write: Wire, pc_increment: Wire, read_input: Wire, write_output: Wire, reset_cycle: Register, halt: Wire,
            cir: Register, clock: Register, ccr: Register, fe_status: Register
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

        # Inputs
        self.cir = cir
        self.clock = clock
        self.ccr = ccr
        self.fe_status = fe_status

    def run(self):
        match self.fe_status.data:
            case 0:
                self.prefetch()
            case 1:
                self.fetch()
            case 2:
                self.execute_1()
            case 3:
                self.execute_2()

    def prefetch(self):
        # Send pc to mar.
        self.pc_write.enable()
        self.mar_enable.enable()

        self.rw_bus.set_data(1)

        # Increment pc
        self.pc_increment.enable()

    def fetch(self):
        self.main_store_enable.enable()

        self.cir_read.enable()

    def execute_1(self):
        print(self.cir.data)

    def execute_2(self):
        if self.cir.data == 0:
            self.halt.enable()
