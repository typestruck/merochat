const { webcrypto } = await import('node:crypto');

export function randomInts_(n, min, max) {
      return shuffle([...Array(max).keys()].map(i => i + min)).slice(0, n - 1);
}

//returns random integer r with equal chance in min <= r < max.
export function randomInt_(min, max) {
      let range = max - min,
            requestBytes = Math.ceil(Math.log2(range) / 8),
            maxNum = Math.pow(256, requestBytes),
            ar = new Uint8Array(requestBytes);

      while (true) {
            webcrypto.getRandomValues(ar);

            let val = 0;

            for (let i = 0; i < requestBytes; i++) {
                  val = (val << 8) + ar[i];
            }

            if (val < maxNum - maxNum % range) {
                  return min + (val % range);
            }
      }
}

//shuffle an array using knuth shuffle
function shuffle(array) {
      let currentIndex = array.length, randomIndex;

      while (currentIndex != 0) {
            randomIndex = randomInt_(0, currentIndex);
            currentIndex--;

            [array[currentIndex], array[randomIndex]] = [array[randomIndex], array[currentIndex]];
      }

      return array;
}
