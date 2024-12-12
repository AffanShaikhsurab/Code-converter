```python
# File1.cbl to Python

def main_logic():
    var1 = 1000
    var2 = 2000
    print("File1: Start of program")
    add_vars(var1, var2)


def add_vars(var1, var2):
    var1 += var2
    print(f"File1: VAR1 after addition = {var1}")

if __name__ == "__main__":
    main_logic()
```