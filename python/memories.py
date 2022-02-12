import json

if __name__ == "__main__":
    with open("python/output.json", "w") as f:
        json.dump("test", f)
