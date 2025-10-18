import opcode

from cu import Opcode


class OpcodeError(Exception):
    pass


class ParamError(Exception):
    pass

class LineTooLongError(Exception):
    pass

class Assembler:
    """
    STR(I): 0 = R1, 1 = R2
    MOV: 10 = R1, 11 = R2, 00 = Zero, 01 = Acc
    """

    def __init__(self, path):
        self.path = path
        self.labels = {}

    def add_label(self, label, param, line_num):
        param = param.strip()
        if len(param) == 0:
            self.labels[label] = format(line_num, 'b')
        else:
            self.labels[label] = format(int(param), 'b')

    def opcode_param_to_bin(self, opcode, param):
        # Takes a standard parameter (R1, R2, ACC) and an opcode and converts that to bin.

        if self.labels.get(param, None) is not None:
            thingy = self.labels[param]
            if opcode.name == "ADD" or opcode.name == "SUB":
                return "1" + thingy.zfill(11)
            return self.labels[param].zfill(12)

        other_thingy = None
        match opcode.name:
            case "MOV":
                match param.lower():
                    case "r1":
                        return "10"
                    case "r2":
                        return "11"
                    case "acc":
                        return "01"
                    case "zero":
                        return "00"
            case "ADD" | "SUB":
                match param.lower():
                    case "r1":
                        return "00"
                    case "r2":
                        return "01"
                    case _:
                        return "1" + format(int(param), "b").zfill(11)
            case "LS":
                match param.lower():
                    case "l":
                        return "0"
                    case "r":
                        return "1"
                    case _:
                        if int(param) > 16:
                            raise ParamError("Param too big for logical shift.")
                        return format(int(param), "b").ljust(11, "0")
            case _:
                match param.lower():
                    case "r1":
                        other_thingy = "0"
                    case "r2":
                        other_thingy = "1"

        if other_thingy is not None:
            return other_thingy + ("1" * 11)

        return format(int(param), "b").zfill(12)

    def process_line(self, line, line_num):
        """
        Take a line, and process it.
        returns
        """

        line = line.strip()
        if len(line) == 0:
            return None

        if ":" in line:
            label, param = line.split(":")
            self.add_label(label, param, line_num)
            return None

        c = line.split(" ")

        code, params = c[0], c[1:]

        try:
            opcode = Opcode[code]
        except KeyError:
            raise OpcodeError(f"Unknown opcode {code}")

        # Format the opcode to be 4 bits.
        final = format(opcode.value, 'b').zfill(4)

        # Convert ref to address
        for i, arg in enumerate(params):
            final += self.opcode_param_to_bin(opcode, arg)

        return final.ljust(16, "0")

    def assemble(self):
        with open(self.path, "r") as file:
            lines = file.readlines()

            final = []
            for line in lines:
                n = self.process_line(line, len(final))
                if n is not None:
                    if len(n) > 16:
                        raise LineTooLongError("WTF ARE YOU DOING BRO, THAT BINARY IS NOT HIPPITY SMALL ENOUGH")
                    final.append(n)

            return final

    def run(self):
        bin_lines = self.assemble()
        new_file = open("bin.txt", "w")

        for i in range(len(bin_lines) - 1):
            bin_lines[i] = bin_lines[i] + "\n"

        new_file.writelines(bin_lines)

        path = new_file.name
        new_file.close()

        return path


if __name__ == "__main__":
    assembler = Assembler("/Users/joshdavies/Coding/WHACK/test.txt")
    print(assembler.run())
