let baseScrollHeight;

exports.resizeTextarea_ = function (textarea) {
    let minRows = 1,
        rows;

    if (baseScrollHeight === undefined)
        baseScrollHeight = textarea.scrollHeight;

    textarea.rows = minRows;
    rows = Math.ceil((textarea.scrollHeight - baseScrollHeight) / 16);
    textarea.rows = minRows + rows;
};
