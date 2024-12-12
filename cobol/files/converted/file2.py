import sys

def File2(FILE1_VAR1):
    print("File2: Received VAR1 = ", FILE1_VAR1)
    VAR3 = FILE1_VAR1 * 2
    print("File2: VAR3 after computation = ", VAR3)

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: File2.py <FILE1_VAR1>\n")
        sys.exit(1)
    File2(int(sys.argv[1]))