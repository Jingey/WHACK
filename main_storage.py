from langchain_ollama import OllamaLLM

from wires import Wire, Bus
from logs import log


class MainStorage:
    """
    0 = write, 1 = read.
    """

    def __init__(
            self, enable: Wire, address_bus: Bus, data_bus: Bus, rw_bus: Bus, is_ai: bool
    ):
        self.enable = enable
        self.address_bus = address_bus
        self.data_bus = data_bus
        self.rw_bus = rw_bus
        self.mem = [0] * (2 ** 16)
        self.enable.enlist(self.execute)

    def execute(self):
        if self.rw_bus.read_data() == 1:
            self.read()
        else:
            self.write()

    def read(self):
        addr = self.address_bus.read_data()
        # log.log_ms_load(addr, self.mem[addr])
        self.data_bus.set_data(self.mem[addr])

    def write(self):
        addr = self.address_bus.read_data()
        data = self.data_bus.read_data()
        log.log_ms_save(addr, data)
        self.mem[addr] = data

    def load(self, data):
        for i, v in enumerate(data):
            self.mem[i] = v


class MainStorageLLM:
    def __init__(self):
        self.mem = []
        self.llm = OllamaLLM(model="wizardlm2", temperature=0, reasoning=False)

    def __setitem__(self, key, value):
        block = key // 4
        offset = key % 4
        if block + 1 > len(self.mem):
            for i in range(block + 1 - len(self.mem)):
                self.mem.append([0] * 4)
        self.mem[block][offset] = value

    def __getitem__(self, item):
        result = 0

        block = item // 4
        offset = item % 4

        prompt = f"""Given the array A = {self.mem[block]} and the index {offset}, return only the integer at that index in the array.
        Important: Output only the integer value at that index. Do not include any explanation, text, or punctuation. Just output the integer."""

        while result != self.mem[block][offset]:
            prompt += f" The answer is not: {result}."
            try:
                result = int(self.llm.invoke(prompt))
            except ValueError:
                pass
        return result
