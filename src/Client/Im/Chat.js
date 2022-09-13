let minRows = 1,
    //firefox adds this weird ass padding to text areas that look like a bug (or I don't know how to fix:)
    // so we account for that in the number of rows
    lenience = 1,
    baseScrollHeight,
    fontSize;

export function resizeTextarea_(textarea) {
    let value = textarea.value;

    if (value === '')
        textarea.rows = minRows;
    else {
        if (baseScrollHeight === undefined) {
            baseScrollHeight = textarea.scrollHeight;
            fontSize = parseInt(window.getComputedStyle(textarea).getPropertyValue('font-size'));
        }

        let rows = Math.ceil((textarea.scrollHeight - baseScrollHeight) / fontSize),
            emptyLines = (value.match(/\n\n/g) || []).length + (rows <= 1 ? 1 : 0); //single line of text plus empty needs an extra row
        textarea.rows = Math.max(minRows + rows + emptyLines - lenience, minRows);
    }
}
