import kaggle
import pandas as pd
import json
import os

# Step 1: Get the information from KAGGLE DATA SET
def fetch_kaggle_data():
    dataset = "amalrajsingh/cryptocurrency-blockchain-and-stock-market-qa"
    output_dir = "./kaggle_data"
    os.makedirs(output_dir, exist_ok=True)

    print("Downloading data from Kaggle...")
    kaggle.api.dataset_download_files(dataset, path=output_dir, unzip=True)
    print("Data downloaded successfully!")
    return os.path.join(output_dir, "train.csv")  

# Step 2: Convert the train.csv file to a jsonl output
# We arent using the text_corpus.jsonl bc of the structure.
def convert_csv_to_jsonl(csv_path, jsonl_path):
    print("Converting CSV to JSONL format...")
    df = pd.read_csv(csv_path)

    with open(jsonl_path, "w") as jsonl_file:
        for _, row in df.iterrows():
            record = {
                "messages": [
                    {"role": "user", "content": row["question"]},
                    {"role": "assistant", "content": row["answer"]}
                ]
            }
            jsonl_file.write(json.dumps(record) + "\n")
    
    print(f"JSONL file created successfully at: {jsonl_path}")

if __name__ == "__main__":
    csv_file_path = fetch_kaggle_data()

    jsonl_output_path = "./kaggle_data/train.jsonl"
    convert_csv_to_jsonl(csv_file_path, jsonl_output_path)