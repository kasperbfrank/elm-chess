build:
	npx yarn \
 	&& npx tailwindcss -i ./src/styles.css -o ./dist/output.css \
 	&& npx elm make src/Main.elm --output=dist/main.js

run-live:
	elm-live src/Main.elm --open -- --output=dist/main.js

run-tailwindcss-watch:
	npx tailwindcss -i ./src/styles.css -o ./dist/output.css --watch