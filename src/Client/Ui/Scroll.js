export function scrollIntoView_(element) {
      element.scrollIntoView(true);
}

export function scrollToTop_(id) {
      if (typeof lynx !== "undefined") {
            lynx.createSelectorQuery()
                  .select(`#${id}`)
                  .invoke({ method: "scrollTo", params: { index: 0, offset: 0, smooth: false } })
                  .exec();
      } else if (typeof document !== "undefined") {
            let element = document.getElementById(id);

            if (element)
                  element.scrollTop = 0;
      }
}
