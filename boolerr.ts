function parseBool(text: string) {
    const result = {true: true, false: false}[text];
    if (result == null) {
        throw Error("bad bool")
    }
    return result;
}

function process(text: string) {
    return text ? parseBool(text) : null;
}

function main() {
    for (let text of ["true", "false", "", "bad"]) {
        const processed = process(text);
        console.log(`"${text}" is ${processed}`)
    }
}

main()
