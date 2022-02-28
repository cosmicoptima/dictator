from flask import Flask, jsonify, request
from sentence_transformers import SentenceTransformer, util
from traceback import format_exc

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
            output_dict["debug"].append(
                # {
                #     "memory": memories[top_memory["corpus_id"]],
                #     "score": top_memory["score"],
                # }
                f"Score: {top_memory['score']}\nMemory: {memories[top_memory['corpus_id']]}\nMessage: {last_message}"
            )

        return jsonify(output_dict)
    except Exception as e:
        return jsonify({"debug": [format_exc()]})


if __name__ == "__main__":
    app.run(port=5000)
