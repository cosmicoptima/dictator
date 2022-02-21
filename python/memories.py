from flask import Flask, jsonify, request
from sentence_transformers import SentenceTransformer, util
from time import perf_counter
from traceback import format_exc
import os

model = SentenceTransformer("all-mpnet-base-v2")

app = Flask(__name__)


@app.route("/", methods=["POST"])
def main():
    try:
        input_dict = request.json
        output_dict = {"debug": []}

        memories = input_dict["miMemories"]
        last_message = input_dict["miMessages"][0]

        memory_embeddings = model.encode(memories)
        last_message_embedding = model.encode(last_message)

        top_memories = util.semantic_search(
            last_message_embedding, memory_embeddings, top_k=1
        )[0]
        if len(top_memories) > 0:
            top_memory = top_memories[0]
            output_dict["memory"] = memories[top_memory["corpus_id"]]

        return jsonify(output_dict)
    except Exception as e:
        return jsonify({"debug": [format_exc()]})


if __name__ == "__main__":
    os.nice(10)
    app.run(port=5000)
