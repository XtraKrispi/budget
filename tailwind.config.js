/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./lib/**/*.{html,js,hs}", "./app/**/*.{html,js,hs}"],
  theme: {
    extend: {},
  },
  plugins: [require('@tailwindcss/typography'), require('daisyui')],
}

