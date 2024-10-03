/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./src/**/*.{html,js,hs}"],
  theme: {
    extend: {},
  },
  plugins: [require('@tailwindcss/typography'), require('daisyui')],
}

