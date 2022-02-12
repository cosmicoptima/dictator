import json
from sentence_transformers import SentenceTransformer, util

model = SentenceTransformer("all-mpnet-base-v2")


def main():
    with open("python/input.json", "r") as f:
        data = json.load(f)

    memories = data["miMemories"]
    last_message = data["miMessages"][0]

    memory_embeddings = model.encode(memories)
    last_message_embedding = model.encode(last_message)

    top_memories = util.semantic_search(
        last_message_embedding, memory_embeddings, top_k=1
    )
    if len(top_memories) > 0:
        top_memory = top_memory[0]

        with open("python/output.json", "w") as f:
            json.dump({"memory": top_memory["corpus_id"]}, f)
    else:
        # oh no
        with open("python/output.json", "w") as f:
            json.dump({}, f)


if __name__ == "__main__":
    main()
