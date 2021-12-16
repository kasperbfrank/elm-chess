run:
	elm-live src/Main.elm --open -- --output=dist/main.js

run-tailwindcss-watch:
	npx tailwindcss -i ./src/styles.css -o ./dist/output.css --watch