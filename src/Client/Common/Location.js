exports.getParameter_ = function(search, name) {
      return (new URLSearchParams(search)).get(name) || "";
}