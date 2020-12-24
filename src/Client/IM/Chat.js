let minRows = 1,
    lenience,
    baseScrollHeight,
    fontSize;

exports.resizeTextarea_ = function (textarea) {
    let rows;

    if (baseScrollHeight === undefined) {
        baseScrollHeight = textarea.scrollHeight;
        fontSize = parseInt(window.getComputedStyle(textarea).getPropertyValue('font-size'));
        //firefox adds this weird ass padding to textareas that look like a bug (or I don't know how to fix:)
        // so we account for that in the number of rows
        lenience = window.navigator.userAgent.includes("Firefox") ? 0 : 1;
    }

    textarea.rows = minRows;
    rows = Math.ceil((textarea.scrollHeight - baseScrollHeight) / fontSize);
    textarea.rows = Math.max(minRows + rows - lenience, minRows);
};
