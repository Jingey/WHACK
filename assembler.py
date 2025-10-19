import codecs

from cu import Opcode


class OpcodeError(Exception):
    pass


class ParamError(Exception):
    pass


class LineTooLongError(Exception):
    pass


class InvalidLabelNameError(Exception):
    pass


invalid_labels = {"r1", "r2", "zero", "acc", "r", "w", "push", "pop"}.union(
    {i.value for i in Opcode}
)


class UnevaluatedLabel:
    def __init__(self, value: list[str]):
        self.value = value

    def pre_add(self, value):
        self.value = [value] + self.value

    def add_str(self, value):
        self.value.append(value)

    def evaluate(self, labels):
        left = ""
        right = ""
        label = ""

        for i in self.value:
            if i in labels:
                label = labels[i]
            elif label == "":
                left += i
            else:
                right += i

        total_len = len(left) + len(right)

        label = label.rjust(16 - total_len, "0")
        return left + label + right


class Assembler:
    """
    STR(I): 0 = R1, 1 = R2
    MOV: 10 = R1, 11 = R2, 00 = Zero, 01 = Acc
    """

    def __init__(self, path):
        self.path = path
        self.labels = {}

    def add_label(self, label, param, line_num):
        if not self.is_label(label):
            raise InvalidLabelNameError()
        param = param.strip()
        if len(param) == 0:
            self.labels[label] = format(line_num, "b")
        else:
            self.labels[label] = format(int(param), "b")

    def is_label(self, param):
        try:
            int(param)
            return False
        except:
            pass
        return param.lower() not in invalid_labels

    def opcode_param_to_bin(self, opcode, param, cur):
        # Takes a standard parameter (R1, R2, ACC) and an opcode and converts that to bin.

        if self.is_label(param):
            if opcode.name == "ADD" or opcode.name == "SUB":
                return UnevaluatedLabel(["1", param])
            return UnevaluatedLabel([param])

        other_thingy = None
        match opcode.name:
            case "MOV":
                pre_pend = "" if len(cur) == 4 else "1"
                match param.lower():
                    case "r1":
                        return pre_pend + "10"
                    case "r2":
                        return pre_pend + "11"
                    case "acc":
                        return pre_pend + "00"
                    case _:
                        return format(int(param), "b").zfill(10)
            case "ADD" | "SUB" | "OR" | "AND":
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
            case "IO" | "STK":
                match param.lower():
                    case "r" | "push":
                        return "0"
                    case "w" | "pop":
                        return "1"
                    case "r1":
                        return "0110"
                    case "r2":
                        return "0111"
                    case "acc":
                        return "010"
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
        line = line.split("//")[0].strip()
        if len(line) == 0:
            return None

        if line[0] == '"' and line[-1] == '"':
            return [
                format(ord(i), "b").rjust(16, "0")
                for i in codecs.getdecoder("unicode_escape")(line[1:-1])[0]
            ]

        try:
            return [format(int(line), "b").rjust(16, "0")]
        except:
            pass

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
        final = format(opcode.value, "b").zfill(4)

        # Convert ref to address
        for i, arg in enumerate(params):
            result = self.opcode_param_to_bin(opcode, arg, final)
            if isinstance(result, UnevaluatedLabel):
                if isinstance(final, UnevaluatedLabel):
                    for i in result.value:
                        final.add_str(i)
                else:
                    result.pre_add(final)
                    final = result
            else:
                if isinstance(final, UnevaluatedLabel):
                    final.add_str(result)
                else:
                    final += result

        return [final]

    def assemble(self):
        with open(self.path, "r") as file:
            lines = file.readlines()

            final = []
            for line in lines:
                arr = self.process_line(line, len(final))
                if arr is None:
                    continue

                for n in arr:
                    final.append(n)

            final = [
                i.evaluate(self.labels) if isinstance(i, UnevaluatedLabel) else i
                for i in final
            ]

            for n in final:
                if len(n) > 16:
                    raise LineTooLongError(
                        "WTF ARE YOU DOING BRO, THAT BINARY IS NOT HIPPITY SMALL ENOUGH"
                    )

            return [i.ljust(16, "0") for i in final]

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
