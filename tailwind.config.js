/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./src/**/*.elm"],
  theme: {
    extend: {
      fontSize: {
        sm: ["0.9rem", { lineHeight: "1.5rem" }],
      },
    },
  },
  plugins: [],
}

