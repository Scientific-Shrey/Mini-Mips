
# Import necessary libraries
import numpy as np

# Define constants
MEMORY_SIZE = 1024
WORD_SIZE = 32

# Initialize memory
memory = np.zeros((MEMORY_SIZE, WORD_SIZE), dtype=int)

# Function to load instructions into memory
def load_instructions(instructions):
    for i, instruction in enumerate(instructions):
        if i < MEMORY_SIZE:
            memory[i] = instruction
        else:
            print("Memory overflow. Instruction not loaded.")

# Function to fetch instruction from memory
def fetch_instruction(address):
    if 0 <= address < MEMORY_SIZE:
        return memory[address]
    else:
        print("Invalid address. Fetch failed.")
        return None

# Example usage
if __name__ == "__main__":
    # Example instructions
    example_instructions = [
        [1, 0, 0, 1],  # Example binary instruction
        [0, 1, 1, 0],  # Example binary instruction
    ]

    # Load instructions into memory
    load_instructions(example_instructions)

    # Fetch an instruction
    fetched_instruction = fetch_instruction(0)
    print("Fetched instruction:", fetched_instruction)