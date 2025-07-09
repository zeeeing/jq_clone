# Haskell `jq` Clone – CSE3100 Functional Programming (TU Delft)

This project is a simplified clone of [`jq`](https://jqlang.org/), a powerful command-line JSON processor, built using the **Haskell programming language** as part of the **CSE3100 Functional Programming** course at **TU Delft**.

📌 **Final Score:** 77 / 100
*The score reflects a strong implementation of core features, with some advanced functionalities left for future development.*

## 📄 Assignment

The full assignment specification can be found in [`ASSIGNMENT.md`](./ASSIGNMENT.md).

## ⚠️ Note on Functionality

This implementation delivers a comprehensive subset of the assignment’s core features. While it robustly supports key functionalities, some advanced requirements and edge cases from the full specification remain outside the current scope.

## 🚀 Usage

To run the program and query a JSON string:

```bash
echo '{"this" : "that"}' | cabal run -- '.this'
```

## 🌐 References

- [Try jq online](https://play.jqlang.org/)
- [jq manual](https://jqlang.org/manual/)
