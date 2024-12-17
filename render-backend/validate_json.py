import json

def validate_jsonl(file_path):
    try:
        with open(file_path, 'r') as file:
            for line_num, line in enumerate(file, 1):
                try:
                    json.loads(line)
                except json.JSONDecodeError as e:
                    print(f"Error in line {line_num}: {e}")
                    return False
        print("Validation passed! The JSONL file is properly formatted.")
        return True
    except Exception as e:
        print(f"Error while validating file: {e}")
        return False

# Validate the JSONL file
validate_jsonl('./kaggle_data/train.jsonl')