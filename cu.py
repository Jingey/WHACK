from enum import Enum
from alu import ALUFunction
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
    INP = 0b1101
    OUT = 0b1110
    HLT = 0b1111


class Cu:
    def __init__(
        self,
        cu_bus: Bus,
        cu_bus_output: Wire,
        func_bus: Bus,
        alu_enable: Wire,
        acc_read: Wire,
        acc_write: Wire,
        r1_read: Wire,
        r1_write: Wire,
        r2_read: Wire,
        r2_write: Wire,
        mar_enable: Wire,
        main_store_enable: Wire,
        rw_bus: Bus,
        cir_read: Wire,
        cir_write: Wire,
        pc_read: Wire,
        pc_write: Wire,
        pc_increment: Wire,
        read_input: Wire,
        write_output: Wire,
        halt: Wire,
        cir: Register,
        clock: Wire,
        ccr: Bus,
        fe_status: Register,
    ):
        self.cu_bus = cu_bus
        self.cu_bus_output = cu_bus_output
        self.func_bus = func_bus
        self.alu_enable = alu_enable
        self.acc_read = acc_read
        self.acc_write = acc_write

        # General purpose registers
        self.r1_read = r1_read
        self.r1_write = r1_write
        self.r2_read = r2_read
        self.r2_write = r2_write

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

        self.halt = halt

        # Inputs
        self.cir = cir
        self.clock = clock
        self.ccr = ccr
        self.fe_status = fe_status

        self.clock.enlist(self.run)

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
        instruction = self.cir.data
        op_code = instruction >> 12
        operand = instruction & 0b1111_11111111

        match op_code:
            case Opcode.HLT.value:
                self.halt_1()
            case Opcode.JMP.value:
                self.jmp_1(operand)
            case Opcode.JEZ.value:
                self.jmp_ez_1(operand)
            case Opcode.JNV.value:
                self.jmp_nvg_1(operand)
            case Opcode.STR.value:
                self.str_1(operand)
            case Opcode.LDR.value:
                self.ldr_1(operand)
            case Opcode.MOV.value:
                self.mov_1(operand)
            case Opcode.LS.value:
                self.ls_1(operand)
            case Opcode.ADD.value:
                self.add_1(operand)
            case Opcode.SUB.value:
                self.sub_1(operand)
            case Opcode.AND.value:
                self.and_1(operand)
            case Opcode.OR.value:
                self.or_1(operand)
            case Opcode.NOT.value:
                self.not_1(operand)
            case Opcode.INP.value:
                self.inp_1(operand)
            case Opcode.OUT.value:
                self.out_1(operand)

            case _:
                # NOP
                return

    def execute_2(self):
        instruction = self.cir.data
        op_code = instruction >> 12
        operand = instruction & 0b1111_11111111
        match op_code:
            case Opcode.STR.value:
                self.str_2(operand)
            case Opcode.LDR.value:
                self.ldr_2(operand)

    def halt_1(self):
        self.halt.enable()

    def write_to_main_bus(self, data):
        self.cu_bus.set_data(data)
        self.cu_bus_output.enable()

    def jmp_1(self, operand):
        # load the address onto the main bus
        self.write_to_main_bus(operand)
        # read the main bus into the program counter
        self.pc_read.enable()

    def jmp_ez_1(self, operand):
        # if last calculation is zero
        if self.ccr.read_data() & 0b100 == 0:
            self.jmp_1(operand)

    def jmp_nvg_1(self, operand):
        # if last calculation is negative
        if self.ccr.read_data() & 0b001 == 0:
            self.jmp_1(operand)

    def send_to_register(self, destination_register):
        if destination_register == 0:
            self.r1_read.enable()
        else:
            self.r2_read.enable()

    def send_from_register(self, source_register):
        if source_register == 0:
            self.r1_write.enable()
        else:
            self.r2_write.enable()

    def load_into_mar(self, addr):
        self.write_to_main_bus(addr)
        self.mar_enable.enable()

    def load_acc_into_mar(self):
        self.acc_write.enable()
        self.mar_enable.enable()

    def str_ldr_address_handling(self, operand):
        addr = operand & ((1 << 11) - 1)

        if addr == 0b111_11111111:
            self.load_acc_into_mar()
        else:
            self.load_into_mar(addr)

    def str_1(self, operand):
        self.str_ldr_address_handling(operand)

        self.rw_bus.set_data(0)

    def str_2(self, operand):
        self.send_from_register((operand >> 11) & 1)

        self.main_store_enable.enable()

    def ldr_1(self, operand):
        self.str_ldr_address_handling(operand)

        self.rw_bus.set_data(1)

    def ldr_2(self, operand):
        self.main_store_enable.enable()

        self.send_to_register((operand >> 11) & 1)

    # from first -> second
    def mov_1(self, operand):
        first_reg = (operand >> 10) & 0b11
        second_reg = (operand >> 8) & 0b11

        if second_reg == 0:
            return

        match first_reg:
            case 0b00:
                self.write_to_main_bus(0)
            case 0b01:
                self.acc_write.enable()
            case 0b10:
                self.r1_write.enable()
            case 0b11:
                self.r2_write.enable()

        match second_reg:
            case 0b01:
                self.acc_read.enable()
            case 0b10:
                self.r1_read.enable()
            case 0b11:
                self.r2_read.enable()

    def ls_1(self, operand):
        shift_amount = operand >> (12 - 5)
        self.write_to_main_bus(shift_amount)
        self.func_bus.set_data(ALUFunction.SHIFT.value)
        self.alu_enable.enable()

    def binary_operation(self, function, operand):
        form = operand >> 11
        if form == 0:
            self.send_from_register((operand >> 10 & 0b1))
        else:
            self.write_to_main_bus(operand & ((1 << 11) - 1))

        self.func_bus.set_data(function.value)
        self.alu_enable.enable()

    def add_1(self, operand):
        self.binary_operation(ALUFunction.ADD, operand)

    def sub_1(self, operand):
        self.binary_operation(ALUFunction.SUB, operand)

    def and_1(self, operand):
        self.binary_operation(ALUFunction.AND, operand)

    def or_1(self, operand):
        self.binary_operation(ALUFunction.OR, operand)

    def not_1(self, operand):
        self.func_bus.set_data(ALUFunction.NOT.value)
        self.alu_enable.enable()

    def inp_1(self, operand):
        self.read_input.enable()

        self.send_to_register((operand >> 11) & 1)

    def out_1(self, operand):
        self.send_from_register((operand >> 11) & 1)

        self.write_output.enable()
