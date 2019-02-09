export function get(url, data: {}, success: (r) => void, failure?: (r) => void): Promise<{}> {
    return request('GET', url, data, success, failure);
}

export function post(url: string, data: {}, success: (r) => void, failure?: (r) => void): Promise<{}> {
    return request('POST', url, data, success, failure);
}

function request(method: 'GET' | 'POST', url: string, data: {}, success = r => { }, failure = error => alert(error ? error.errorMessage : 'An error occured')): Promise<{}> {
    return new Promise(
        function (go, reject) {
            let xmlhttp = new XMLHttpRequest();

            xmlhttp.onload = () => {
                loading(false);

                if (xmlhttp.status >= 200 && xmlhttp.status < 400) {
                    success(JSON.parse(xmlhttp.response));
                    go();
                } else {
                    failure(JSON.parse(xmlhttp.response));
                    if (reject)
                        reject();
                }
            }

            xmlhttp.onerror = e => {
                loading(false);

                failure(e);

                if (reject)
                    reject();
            }

            loading();

            data = Object.keys(data).map(function (c) {
                if (data[c] !== undefined && data[c] !== null)
                    return `${c}=${encodeURIComponent(data[c])}`;
                return `${c}=`;
            }).join('&');

            if (method == 'POST') {
                xmlhttp.open(method, url, true);
                xmlhttp.setRequestHeader('content-type', 'application/x-www-form-urlencoded');
                xmlhttp.setRequestHeader('x-access-token', token());
                xmlhttp.send(data);
            } else {
                if (data)
                    url = `${url}${(url.includes('?') ? '&' : '?')}${data}`;

                xmlhttp.open(method, url, true);
                xmlhttp.send();
            }
        });
}

function loading(show = true) {
    (document.querySelector('#loading') as HTMLDivElement).style.display = show ? 'block' : 'none';
}

declare var grecaptcha;

export function registerOrLogin(url: string, captcha = false, captchaResponse = '') {
    let email = (document.querySelector('#email') as HTMLInputElement).value,
        password = (document.querySelector('#password') as HTMLInputElement).value;

    if (!email || !password) {
        alert('Email and password are mandatory');
        event.preventDefault();
    } else if (captcha)
        grecaptcha.execute();
    else
        post(url, {
            email: email,
            password: password,
            captchaResponse: captchaResponse
        }, token => {
            //tokenPOST is a mitigation for csrf/cookie interception (since zinc http doesn't seem to offer any sort of antiforgery tokens) used for post requests, whereas tokenGET is used for (login restricted) get requests, since I don't to make it a single page application 
            document.cookie = `melanchat=${token.tokenGET};max-age=3471300000;path=/`;
            localStorage.setItem('token', token.tokenPOST);
            location.href = (new URLSearchParams(document.location.search.substring(1))).get('next') || '/im';
        }, error => {
            if (window['grecaptcha'])
                grecaptcha.reset();
            alert(error.errorMessage);
        });
}

export function getMonth1() {
    return this.getMonth() + 1;
}

export function age(date: string | Date): number {
    if (typeof (date) == 'string')
        date = new Date(date);

    if (date && date.getFullYear() > 1000)
        return Math.floor(Math.floor((new Date().getTime() - date.getTime()) / 1000) / (24 * 60 * 60 * 365));

    return 0;
}

export function countryName(countryID: number): string {
    let country = countryList.find(c => c.id == countryID);

    if (country)
        return country.name;

    return '';
}

export function languageNames(languages: number[]): string[] {
    return languages.map(c => languageList.find(l => l.id == c).name);
}

export function token() {
    return localStorage['token'] || '';
}

export function logout() {    
    document.cookie = 'melanchat=; Max-Age=-99999999;'
    localStorage.clear();
    location.href = '/';
}

export let countryList = [{ id: 1, name: "Afghanistan" },
{ id: 2, name: "Albania" },
{ id: 3, name: "Algeria" },
{ id: 4, name: "Andorra" },
{ id: 5, name: "Angola" },
{ id: 6, name: "Antigua and Barbuda" },
{ id: 7, name: "Argentina" },
{ id: 8, name: "Armenia" },
{ id: 9, name: "Australia" },
{ id: 10, name: "Austria" },
{ id: 11, name: "Azerbaijan" },
{ id: 12, name: "Bahamas, he" },
{ id: 13, name: "Bahrain" },
{ id: 14, name: "Bangladesh" },
{ id: 15, name: "Barbados" },
{ id: 16, name: "Belarus" },
{ id: 17, name: "Belgium" },
{ id: 18, name: "Belize" },
{ id: 19, name: "Benin" },
{ id: 20, name: "Bhutan" },
{ id: 21, name: "Bolivia" },
{ id: 22, name: "Bosnia and Herzegovina" },
{ id: 23, name: "Botswana" },
{ id: 24, name: "Brazil" },
{ id: 25, name: "Brunei Darussalam" },
{ id: 26, name: "Bulgaria" },
{ id: 27, name: "Burkina Faso" },
{ id: 28, name: "Burma" },
{ id: 29, name: "Burundi" },
{ id: 30, name: "Cambodia" },
{ id: 31, name: "Cameroon" },
{ id: 32, name: "Canada" },
{ id: 33, name: "Cape Verde" },
{ id: 34, name: "Central African Republic" },
{ id: 35, name: "Chad" },
{ id: 36, name: "Chile" },
{ id: 37, name: "China" },
{ id: 38, name: "Colombia" },
{ id: 39, name: "Comoros" },
{ id: 40, name: "Congo(Brazzaville)" },
{ id: 41, name: "Congo(Kinshasa)" },
{ id: 42, name: "Costa Rica" },
{ id: 43, name: "Cote d'Ivoire" },
{ id: 44, name: "Croatia" },
{ id: 45, name: "Cuba" },
{ id: 46, name: "Cyprus" },
{ id: 47, name: "Czech Republic" },
{ id: 48, name: "Denmark" },
{ id: 49, name: "Djibouti" },
{ id: 50, name: "Dominica" },
{ id: 51, name: "Dominican Republic" },
{ id: 52, name: "East Timor" },
{ id: 53, name: "Ecuador" },
{ id: 54, name: "Egypt" },
{ id: 55, name: "El Salvador" },
{ id: 56, name: "Equatorial Guinea" },
{ id: 57, name: "Eritrea" },
{ id: 58, name: "Estonia" },
{ id: 59, name: "Ethiopia" },
{ id: 60, name: "Fiji" },
{ id: 61, name: "Finland" },
{ id: 62, name: "France" },
{ id: 63, name: "Gabon" },
{ id: 64, name: "Gambia, he" },
{ id: 65, name: "Georgia" },
{ id: 66, name: "Germany" },
{ id: 67, name: "Ghana" },
{ id: 68, name: "Greece" },
{ id: 69, name: "Grenada" },
{ id: 70, name: "Guatemala" },
{ id: 71, name: "Guinea" },
{ id: 72, name: "Guinea-Bissau" },
{ id: 73, name: "Guyana" },
{ id: 74, name: "Haiti" },
{ id: 75, name: "Holy See" },
{ id: 76, name: "Honduras" },
{ id: 77, name: "Hong Kong" },
{ id: 78, name: "Hungary" },
{ id: 79, name: "Iceland" },
{ id: 80, name: "India" },
{ id: 81, name: "Indonesia" },
{ id: 82, name: "Iran" },
{ id: 83, name: "Iraq" },
{ id: 84, name: "Ireland" },
{ id: 85, name: "Israel" },
{ id: 86, name: "Italy" },
{ id: 87, name: "Jamaica" },
{ id: 88, name: "Japan" },
{ id: 89, name: "Jordan" },
{ id: 90, name: "Kazakhstan" },
{ id: 91, name: "Kenya" },
{ id: 92, name: "Kiribati" },
{ id: 93, name: "Korea, orth" },
{ id: 94, name: "Korea, outh" },
{ id: 95, name: "Kosovo" },
{ id: 96, name: "Kuwait" },
{ id: 97, name: "Kyrgyzstan" },
{ id: 98, name: "Laos" },
{ id: 99, name: "Latvia" },
{ id: 100, name: "Lebanon" },
{ id: 101, name: "Lesotho" },
{ id: 102, name: "Liberia" },
{ id: 103, name: "Libya" },
{ id: 104, name: "Liechtenstein" },
{ id: 105, name: "Lithuania" },
{ id: 106, name: "Luxembourg" },
{ id: 107, name: "Macau" },
{ id: 108, name: "Macedonia" },
{ id: 109, name: "Madagascar" },
{ id: 110, name: "Malawi" },
{ id: 111, name: "Malaysia" },
{ id: 112, name: "Maldives" },
{ id: 113, name: "Mali" },
{ id: 114, name: "Malta" },
{ id: 115, name: "Marshall Islands" },
{ id: 116, name: "Mauritania" },
{ id: 117, name: "Mauritius" },
{ id: 118, name: "Mexico" },
{ id: 119, name: "Micronesia" },
{ id: 120, name: "Moldova" },
{ id: 121, name: "Monaco" },
{ id: 122, name: "Mongolia" },
{ id: 123, name: "Montenegro" },
{ id: 124, name: "Morocco" },
{ id: 125, name: "Mozambique" },
{ id: 126, name: "Namibia" },
{ id: 127, name: "Nauru" },
{ id: 128, name: "Nepal" },
{ id: 129, name: "Netherlands" },
{ id: 130, name: "Netherlands Antilles" },
{ id: 131, name: "New Caledonia" },
{ id: 132, name: "New Zealand" },
{ id: 133, name: "Nicaragua" },
{ id: 134, name: "Niger" },
{ id: 135, name: "Nigeria" },
{ id: 136, name: "North Korea" },
{ id: 137, name: "Norway" },
{ id: 138, name: "Oman" },
{ id: 139, name: "Pakistan" },
{ id: 140, name: "Palau" },
{ id: 141, name: "Palestinian Territories" },
{ id: 142, name: "Panama" },
{ id: 143, name: "Papua New Guinea" },
{ id: 144, name: "Paraguay" },
{ id: 145, name: "Peru" },
{ id: 146, name: "Philippines" },
{ id: 147, name: "Poland" },
{ id: 148, name: "Portugal" },
{ id: 149, name: "Qatar" },
{ id: 150, name: "Romania" },
{ id: 151, name: "Russia" },
{ id: 152, name: "Rwanda" },
{ id: 153, name: "Saint Kitts and Nevis" },
{ id: 154, name: "Saint Lucia" },
{ id: 155, name: "St. Vincent/Grenadines" },
{ id: 156, name: "Samoa" },
{ id: 157, name: "SanMarino" },
{ id: 158, name: "Sao Tome and Principe" },
{ id: 159, name: "SaudiA rabia" },
{ id: 160, name: "Senegal" },
{ id: 161, name: "Serbia" },
{ id: 162, name: "Seychelles" },
{ id: 163, name: "Sierra Leone" },
{ id: 164, name: "Singapore" },
{ id: 165, name: "Slovakia" },
{ id: 166, name: "Slovenia" },
{ id: 167, name: "Solomon Islands" },
{ id: 168, name: "Somalia" },
{ id: 169, name: "South Africa" },
{ id: 170, name: "South Korea" },
{ id: 171, name: "Spain" },
{ id: 172, name: "Sri Lanka" },
{ id: 173, name: "Sudan" },
{ id: 174, name: "Suriname" },
{ id: 175, name: "Swaziland" },
{ id: 176, name: "Sweden" },
{ id: 177, name: "Switzerland" },
{ id: 178, name: "Syria" },
{ id: 179, name: "Taiwan" },
{ id: 180, name: "Tajikistan" },
{ id: 181, name: "Tanzania" },
{ id: 182, name: "Thailand" },
{ id: 183, name: "Timor-Leste" },
{ id: 184, name: "Togo" },
{ id: 185, name: "Tonga" },
{ id: 186, name: "Trinidad and Tobago" },
{ id: 187, name: "Tunisia" },
{ id: 188, name: "Turkey" },
{ id: 189, name: "Turkmenistan" },
{ id: 190, name: "Tuvalu" },
{ id: 191, name: "Uganda" },
{ id: 192, name: "Ukraine" },
{ id: 193, name: "United Arab Emirates" },
{ id: 194, name: "United Kingdom" },
{ id: 195, name: "United States" },
{ id: 196, name: "Uruguay" },
{ id: 197, name: "Uzbekistan" },
{ id: 198, name: "Vanuatu" },
{ id: 199, name: "Venezuela" },
{ id: 200, name: "Vietnam" },
{ id: 201, name: "Yemen" },
{ id: 202, name: "Zambia" },
{ id: 203, name: "Zimbabwe" }];

export let languageList = [{ id: 1, name: "Albanian" },
{ id: 2, name: "Amharic" },
{ id: 3, name: "Arabic" },
{ id: 4, name: "Arapaho" },
{ id: 5, name: "Armenian" },
{ id: 6, name: "Awadhi" },
{ id: 7, name: "Azerbaijani-South" },
{ id: 8, name: "Basque" },
{ id: 9, name: "Bengali" },
{ id: 10, name: "Bhojpuri" },
{ id: 11, name: "Bosnian" },
{ id: 12, name: "Breton" },
{ id: 13, name: "Burmese" },
{ id: 14, name: "Bulgarian" },
{ id: 15, name: "Catalan" },
{ id: 16, name: "Chinese-Gan" },
{ id: 17, name: "Chinese-Hakka" },
{ id: 18, name: "Chinese-Jinyu" },
{ id: 19, name: "Chinese-Mandarin" },
{ id: 20, name: "Chinese-MinNan" },
{ id: 21, name: "Chinese-Wu" },
{ id: 22, name: "Chinese-Xiang" },
{ id: 23, name: "Chinese-Yue(Cantonese)" },
{ id: 24, name: "Cornish" },
{ id: 25, name: "Croatian" },
{ id: 26, name: "Czech" },
{ id: 27, name: "Danish" },
{ id: 28, name: "Dutch" },
{ id: 29, name: "English" },
{ id: 30, name: "Esperanto" },
{ id: 31, name: "Estonian" },
{ id: 32, name: "Euchee/Yuchi" },
{ id: 33, name: "Faroese" },
{ id: 34, name: "Finnish" },
{ id: 35, name: "French" },
{ id: 36, name: "Galician" },
{ id: 37, name: "German" },
{ id: 38, name: "Georgian" },
{ id: 39, name: "Greek" },
{ id: 40, name: "Gujarati" },
{ id: 41, name: "Haitian Creole" },
{ id: 42, name: "Hausa" },
{ id: 43, name: "Hebrew" },
{ id: 44, name: "Hindi" },
{ id: 45, name: "Hmong" },
{ id: 46, name: "Hungarian" },
{ id: 47, name: "Icelandic" },
{ id: 48, name: "Igbo" },
{ id: 49, name: "Indonesian" },
{ id: 50, name: "Italian" },
{ id: 51, name: "IrishGaelic" },
{ id: 52, name: "Japanese" },
{ id: 53, name: "Javanese" },
{ id: 54, name: "Kannada" },
{ id: 55, name: "Khmer" },
{ id: 56, name: "Konkani" },
{ id: 57, name: "Korean" },
{ id: 58, name: "Kurdish" },
{ id: 59, name: "Latin" },
{ id: 60, name: "Latvian" },
{ id: 61, name: "Lithuanian" },
{ id: 62, name: "Lojban" },
{ id: 63, name: "Macedonian" },
{ id: 64, name: "Maithili" },
{ id: 65, name: "Malay" },
{ id: 66, name: "Malayalam" },
{ id: 67, name: "Manx" },
{ id: 68, name: "Marathi" },
{ id: 69, name: "Mongolian" },
{ id: 70, name: "Mongul" },
{ id: 71, name: "Navajo/Dine" },
{ id: 72, name: "Norwegian" },
{ id: 73, name: "Ojibwe/Anishnaabeg" },
{ id: 74, name: "Oriya" },
{ id: 75, name: "Panjabi-Eastern" },
{ id: 76, name: "Panjabi-Western" },
{ id: 77, name: "Papiamentu" },
{ id: 78, name: "Pashto" },
{ id: 79, name: "Persian" },
{ id: 80, name: "Polish" },
{ id: 81, name: "Portuguese" },
{ id: 82, name: "Romanian" },
{ id: 83, name: "Russian" },
{ id: 84, name: "Sanskrit" },
{ id: 85, name: "Sauk" },
{ id: 86, name: "ScotsGaelic" },
{ id: 87, name: "Serbian" },
{ id: 88, name: "Serbo-Croatian" },
{ id: 89, name: "Sign Language" },
{ id: 90, name: "Sindhi" },
{ id: 91, name: "Sioux" },
{ id: 92, name: "Slovak" },
{ id: 93, name: "Slovenian" },
{ id: 94, name: "Spanish" },
{ id: 96, name: "Sunda" },
{ id: 97, name: "Swahili" },
{ id: 98, name: "Swedish" },
{ id: 99, name: "Tagalog" },
{ id: 100, name: "Tamazight" },
{ id: 101, name: "Tamil" },
{ id: 102, name: "Telugu" },
{ id: 103, name: "Thai" },
{ id: 104, name: "Turkish" },
{ id: 105, name: "Twi" },
{ id: 106, name: "Ukrainian" },
{ id: 107, name: "Urdu" },
{ id: 108, name: "Uzbek" },
{ id: 109, name: "Vietnamese" },
{ id: 110, name: "Wampanoag" },
{ id: 111, name: "Welsh" },
{ id: 112, name: "Wolof" },
{ id: 113, name: "Yiddish" },
{ id: 114, name: "Yoruba" },
{ id: 115, name: "Zulu" }];