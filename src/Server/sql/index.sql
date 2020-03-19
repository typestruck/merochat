create table countries
(
    id serial primary key,
    name varchar(100) not null
);

create table users
(
    id serial primary key,
    name varchar(50) not null,
    password char(128) not null,
    joined timestamp default clock_timestamp(),
    email varchar(50) not null,
    birthday timestamp,
    gender char(1),
    headline varchar(200) not null,
    description varchar(10000) not null,
    recentEmoji varchar(60),
    country integer,
    messageOnEnter boolean not null default true,
    constraint genderCheck check (gender in  ('F', 'M', 'O')),
    constraint countryUser foreign key (country) references countries(id)
);

create table messages
(
    id bigserial primary key,
    sender integer not null,
    recipient integer not null,
    date timestamp not null default clock_timestamp(),
    content varchar(10000) not null,
    status smallint not null default 1,
    visualized timestamp,

    constraint statusCheck check (status in (1,2)),
    constraint fromUserMessage foreign key (sender) references users(id),
    constraint toUserMessage foreign key (recipient) references users(id)
);

create table privileges
(
    id serial primary key,
    feature smallint not null,
    description varchar(100),
    quantity integer not null
);

create table tags
(
    id serial primary key,
    name varchar(30) not null,
    unique(name)
);

create table languages
(
    id serial primary key,
    name varchar(30) not null
);

create table badges
(
    id serial primary key,
    kind smallint not null,
    description varchar(100)
);

create table reactions
(
    id serial primary key,
    reward integer not null,
    kind smallint not null,
    description varchar(100)
);

create table blocked
(
    id serial primary key,
    blocker integer not null,
    blocked integer not null,
    constraint blockerUser foreign key (blocker) references users(id),
    constraint blockedUser foreign key (blocked) references users(id)
);

create table reports
(
    id serial primary key,
    description varchar(10000) not null,
    offense integer not null,
    reporter integer not null,
    reported integer not null,
    constraint offenseReaction foreign key (offense) references reactions(id),
    constraint reporterUser foreign key (reporter) references users(id),
    constraint reportedUser foreign key (reported) references users(id)
);

create table privilegesUsers
(
    id serial primary key,
    privilege integer not null,
    receiver integer not null,
    constraint privilegeUserUser foreign key (receiver) references users(id),
    constraint privilegeUserPrivilege foreign key (privilege) references privileges(id)
);

create table tagsUsers
(
    id serial primary key,
    creator integer not null,
    tag integer not null,
    constraint tagUserUser foreign key (creator) references users(id),
    constraint tagUserTag foreign key (tag) references tags(id)
);

create table languagesUsers
(
    id serial primary key,
    speaker integer not null,
    language integer not null,
    constraint languageUserUser foreign key (speaker) references users(id),
    constraint languageUserLanguage foreign key (language) references languages(id)
);

create table badgesUsers
(
    id serial primary key,
    receiver integer not null,
    badge integer not null,
    constraint badgeUserUser foreign key (receiver) references users(id),
    constraint badgeUserBadge foreign key (badge) references badges(id)
);

create table reactionsMessages
(
    id bigserial primary key,
    reaction integer not null,
    message bigint not null,
    constraint reactionMessageMessage foreign key (message) references messages(id),
    constraint reactionMessageReaction foreign key (reaction) references reactions(id)
);

create table reactionsUsers
(
    id bigserial primary key,
    bearer integer not null,
    reaction integer not null,
    constraint reactionUserUser foreign key (bearer) references users(id),
    constraint reactionUserReaction foreign key (reaction) references reactions(id)
);

insert into languages
    (name)
values
    ('Albanian'),
    ('Amharic'),
    ('Arabic'),
    ('Arapaho'),
    ('Armenian'),
    ('Awadhi'),
    ('Azerbaijani-South'),
    ('Basque'),
    ('Bengali'),
    ('Bhojpuri'),
    ('Bosnian'),
    ('Breton'),
    ('Burmese'),
    ('Bulgarian'),
    ('Catalan'),
    ('Chinese-Gan'),
    ('Chinese-Hakka'),
    ('Chinese-Jinyu'),
    ('Chinese-Mandarin'),
    ('Chinese-MinNan'),
    ('Chinese-Wu'),
    ('Chinese-Xiang'),
    ('Chinese-Yue(Cantonese)'),
    ('Cornish'),
    ('Croatian'),
    ('Czech'),
    ('Danish'),
    ('Dutch'),
    ('English'),
    ('Esperanto'),
    ('Estonian'),
    ('Euchee/Yuchi'),
    ('Faroese'),
    ('Finnish'),
    ('French'),
    ('Galician'),
    ('German'),
    ('Georgian'),
    ('Greek'),
    ('Gujarati'),
    ('HaitianCreole'),
    ('Hausa'),
    ('Hebrew'),
    ('Hindi'),
    ('Hmong'),
    ('Hungarian'),
    ('Icelandic'),
    ('Igbo'),
    ('Indonesian'),
    ('Italian'),
    ('IrishGaelic'),
    ('Japanese'),
    ('Javanese'),
    ('Kannada'),
    ('Khmer'),
    ('Konkani'),
    ('Korean'),
    ('Kurdish'),
    ('Latin'),
    ('Latvian'),
    ('Lithuanian'),
    ('Lojban'),
    ('Macedonian'),
    ('Maithili'),
    ('Malay'),
    ('Malayalam'),
    ('Manx'),
    ('Marathi'),
    ('Mongolian'),
    ('Mongul'),
    ('Navajo/Dine'),
    ('Norwegian'),
    ('Ojibwe/Anishnaabeg'),
    ('Oriya'),
    ('Panjabi-Eastern'),
    ('Panjabi-Western'),
    ('Papiamentu'),
    ('Pashto'),
    ('Persian'),
    ('Polish'),
    ('Portuguese'),
    ('Romanian'),
    ('Russian'),
    ('Sanskrit'),
    ('Sauk'),
    ('ScotsGaelic'),
    ('Serbian'),
    ('Serbo-Croatian'),
    ('American Sign Language'),
    ('Sindhi'),
    ('Sioux'),
    ('Slovak'),
    ('Slovenian'),
    ('Spanish'),
    ('StutteringTherapy'),
    ('Sunda'),
    ('Swahili'),
    ('Swedish'),
    ('Tagalog'),
    ('Tamazight'),
    ('Tamil'),
    ('Telugu'),
    ('Thai'),
    ('Turkish'),
    ('Twi'),
    ('Ukrainian'),
    ('Urdu'),
    ('Uzbek'),
    ('Vietnamese'),
    ('Wampanoag'),
    ('Welsh'),
    ('Wolof'),
    ('Yiddish'),
    ('Yoruba'),
    ('Zulu'),
    ('Other');

insert into countries
    (name)
values
    ('Afghanistan'),
    ('Albania'),
    ('Algeria'),
    ('Andorra'),
    ('Angola'),
    ('Antigua and Barbuda'),
    ('Argentina'),
    ('Armenia'),
    ('Australia'),
    ('Austria'),
    ('Azerbaijan'),
    ('Bahamas, he'),
    ('Bahrain'),
    ('Bangladesh'),
    ('Barbados'),
    ('Belarus'),
    ('Belgium'),
    ('Belize'),
    ('Benin'),
    ('Bhutan'),
    ('Bolivia'),
    ('Bosnia and Herzegovina'),
    ('Botswana'),
    ('Brazil'),
    ('Brunei Darussalam'),
    ('Bulgaria'),
    ('Burkina Faso'),
    ('Burma'),
    ('Burundi'),
    ('Cambodia'),
    ('Cameroon'),
    ('Canada'),
    ('Cape Verde'),
    ('Central African Republic'),
    ('Chad'),
    ('Chile'),
    ('China'),
    ('Colombia'),
    ('Comoros'),
    ('Congo(Brazzaville)'),
    ('Congo(Kinshasa)'),
    ('Costa Rica'),
    ('Cote d''Ivoire'),
    ('Croatia'),
    ('Cuba'),
    ('Cyprus'),
    ('Czech Republic'),
    ('Denmark'),
    ('Djibouti'),
    ('Dominica'),
    ('Dominican Republic'),
    ('East Timor'),
    ('Ecuador'),
    ('Egypt'),
    ('El Salvador'),
    ('Equatorial Guinea'),
    ('Eritrea'),
    ('Estonia'),
    ('Ethiopia'),
    ('Fiji'),
    ('Finland'),
    ('France'),
    ('Gabon'),
    ('Gambia, he'),
    ('Georgia'),
    ('Germany'),
    ('Ghana'),
    ('Greece'),
    ('Grenada'),
    ('Guatemala'),
    ('Guinea'),
    ('Guinea-Bissau'),
    ('Guyana'),
    ('Haiti'),
    ('Holy See'),
    ('Honduras'),
    ('Hong Kong'),
    ('Hungary'),
    ('Iceland'),
    ('India'),
    ('Indonesia'),
    ('Iran'),
    ('Iraq'),
    ('Ireland'),
    ('Israel'),
    ('Italy'),
    ('Jamaica'),
    ('Japan'),
    ('Jordan'),
    ('Kazakhstan'),
    ('Kenya'),
    ('Kiribati'),
    ('Korea, orth'),
    ('Korea, outh'),
    ('Kosovo'),
    ('Kuwait'),
    ('Kyrgyzstan'),
    ('Laos'),
    ('Latvia'),
    ('Lebanon'),
    ('Lesotho'),
    ('Liberia'),
    ('Libya'),
    ('Liechtenstein'),
    ('Lithuania'),
    ('Luxembourg'),
    ('Macau'),
    ('Macedonia'),
    ('Madagascar'),
    ('Malawi'),
    ('Malaysia'),
    ('Maldives'),
    ('Mali'),
    ('Malta'),
    ('Marshall Islands'),
    ('Mauritania'),
    ('Mauritius'),
    ('Mexico'),
    ('Micronesia'),
    ('Moldova'),
    ('Monaco'),
    ('Mongolia'),
    ('Montenegro'),
    ('Morocco'),
    ('Mozambique'),
    ('Namibia'),
    ('Nauru'),
    ('Nepal'),
    ('Netherlands'),
    ('Netherlands Antilles'),
    ('New Caledonia'),
    ('New Zealand'),
    ('Nicaragua'),
    ('Niger'),
    ('Nigeria'),
    ('North Korea'),
    ('Norway'),
    ('Oman'),
    ('Pakistan'),
    ('Palau'),
    ('Palestinian Territories'),
    ('Panama'),
    ('Papua New Guinea'),
    ('Paraguay'),
    ('Peru'),
    ('Philippines'),
    ('Poland'),
    ('Portugal'),
    ('Qatar'),
    ('Romania'),
    ('Russia'),
    ('Rwanda'),
    ('Saint Kitts and Nevis'),
    ('Saint Lucia'),
    ('St. Vincent/Grenadines'),
    ('Samoa'),
    ('SanMarino'),
    ('Sao Tome and Principe'),
    ('SaudiA rabia'),
    ('Senegal'),
    ('Serbia'),
    ('Seychelles'),
    ('Sierra Leone'),
    ('Singapore'),
    ('Slovakia'),
    ('Slovenia'),
    ('Solomon Islands'),
    ('Somalia'),
    ('South Africa'),
    ('South Korea'),
    ('Spain'),
    ('Sri Lanka'),
    ('Sudan'),
    ('Suriname'),
    ('Swaziland'),
    ('Sweden'),
    ('Switzerland'),
    ('Syria'),
    ('Taiwan'),
    ('Tajikistan'),
    ('Tanzania'),
    ('Thailand'),
    ('Timor-Leste'),
    ('Togo'),
    ('Tonga'),
    ('Trinidad and Tobago'),
    ('Tunisia'),
    ('Turkey'),
    ('Turkmenistan'),
    ('Tuvalu'),
    ('Uganda'),
    ('Ukraine'),
    ('United Arab Emirates'),
    ('United Kingdom'),
    ('United States'),
    ('Uruguay'),
    ('Uzbekistan'),
    ('Vanuatu'),
    ('Venezuela'),
    ('Vietnam'),
    ('Yemen'),
    ('Zambia'),
    ('Zimbabwe');


create table recoveries
(
    id serial primary key,
    uuid char(36) not null,
    created timestamp default clock_timestamp(),
    active boolean default true,
    recoverer integer not null,
    constraint recoverer foreign key (recoverer) references users(id)
);

create table karmas (
    id serial primary key,
    target integer not null,
    current integer not null,
    
    constraint targetKarma foreign key (target) references users(id)
);

create table histories
(
    id serial primary key,
    sender integer not null,
    recipient integer not null,
    firstMessageDate timestamp not null default clock_timestamp(),
    date timestamp not null default clock_timestamp(),
    senderArchived boolean not null default false,
    recipientArchived boolean not null default false,

    constraint fromUserMessage foreign key (sender) references users(id),
    constraint toUserMessage foreign key (recipient) references users(id),

    unique(sender, recipient)
);

INSERT INTO users
    (
    id, name, password, joined, email, birthday, gender, headline,
    description)
VALUES
    (1, 'bender', 'cant log in with it', clock_timestamp(), 'bender@melan.chat', clock_timestamp(), 'O', 'Here to help you with all your Melanchat queries', 'Shining shiner!');

CREATE OR REPLACE FUNCTION insertHistory
(senderID int, recipientID int)
  RETURNS boolean AS
$BODY$
begin
    if exists(select 1
    from histories
    where sender = senderID or sender = recipientID and recipient = senderID) then
    update histories set senderArchived = false, recipientArchived = false, date = clock_timestamp() where sender = recipientID and recipient = senderID;
    else
    insert into histories
        (sender, recipient)
    values
        (senderID, recipientID)
    on conflict
    (sender, recipient) do
    update set senderArchived = false, recipientArchived = false, date = clock_timestamp();
    return alreadyExists;
end
if;
end;
  $BODY$
  LANGUAGE plpgsql;

-- CREATE OR REPLACE FUNCTION truncatetables()
--   RETURNS void AS
-- $BODY$
-- begin
--         truncate table users  RESTART IDENTITY cascade;
--         truncate table messages RESTART IDENTITY cascade ;
--         truncate table tags RESTART IDENTITY cascade ;
-- end;
--   $BODY$
--   LANGUAGE plpgsql;