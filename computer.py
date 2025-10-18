from wires import Wire, Bus
from inout import Input, Output


class Computer:
    def __init__(
        self,
        clock: Wire,
        halt: Wire,
        input_reg: Input,
        output_reg: Output,
    ):
        self.clock = clock
        self.finished = False
        self.halt = halt
        self.input_reg = input_reg
        self.output_reg = output_reg
        halt.enlist(self.halted)

    def give_input(self, data: int):
        self.input_reg.take_input(data)

    def halted(self):
        self.finished = True

    def run_cycle(self):
        if not self.finished:
            self.clock.enable()

    def run(self):
        while not self.finished:
            self.run_cycle()
