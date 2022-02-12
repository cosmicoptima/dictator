import json
from sentence_transformers import SentenceTransformer


def main():
    with open("python/input.json", "r") as f:
        data = json.load(f)

    with open("python/output.json", "w") as f:
        json.dump({}, f)


if __name__ == "__main__":
    main()
