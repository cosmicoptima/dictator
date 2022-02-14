import json
from sentence_transformers import SentenceTransformer, util
from time import perf_counter


output_dict = {"debug": []}

before_import = perf_counter()
model = SentenceTransformer("all-mpnet-base-v2")
after_import = perf_counter()
output_dict["debug"].append(
    f"Importing model took {after_import - before_import} seconds"
)

with open("python/input.json", "r") as f:
    data = json.load(f)


memories = data["miMemories"]
last_message = data["miMessages"][0]

before_memories_encode = perf_counter()
memory_embeddings = model.encode(memories)
after_memories_encode = perf_counter()
output_dict["debug"].append(
    f"Encoding memories took {after_memories_encode - before_memories_encode} seconds"
)

before_last_message_encode = perf_counter()
last_message_embedding = model.encode(last_message)
after_last_message_encode = perf_counter()
output_dict["debug"].append(
    f"Encoding last message took {after_last_message_encode - before_last_message_encode} seconds"
)

top_memories = util.semantic_search(last_message_embedding, memory_embeddings, top_k=1)[
    0
]
if len(top_memories) > 0:
    top_memory = top_memories[0]
    output_dict["memory"] = memories[top_memory["corpus_id"]]


with open("python/output.json", "w") as f:
    json.dump(output_dict, f)
