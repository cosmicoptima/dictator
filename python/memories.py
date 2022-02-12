import json


def main():
    with open("python/input.json", "r") as f:
        data = json.load(f)

    raise Exception(f"First message: {data[0]}")

    # ...
    with open("python/output.json", "w") as f:
        json.dump({}, f)


if __name__ == "__main__":
    main()
