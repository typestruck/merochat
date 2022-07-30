﻿create or replace function utc_now()
  returns timestamptz as
$body$
begin
    return now() at time zone 'utc';
end;
  $body$
  language plpgsql;

create table countries
(
    id integer generated always as identity primary key,
    name text not null
);

create table users
(
    id integer generated always as identity primary key,
    name text not null,
    password text not null,
    joined timestamptz default (utc_now()),
    email text not null,
    birthday date,
    gender smallint,
    headline text not null,
    avatar text,
    description text not null,
    country integer,
    visibility smallint not null default 0,
    visibility_last_updated timestamptz default (utc_now()),
    read_receipts boolean not null default true,
    typing_status boolean not null default true,
    online_status boolean not null default true,
    message_timestamps boolean not null default true,

    constraint country_user foreign key (country) references countries(id)
);

create table messages
(
    id integer generated always as identity primary key,
    temporary_id integer not null,
    sender integer not null,
    recipient integer not null,
    date timestamptz not null default (now()),
    content text not null,
    status smallint not null default 1,
    visualized timestamptz,

    constraint from_user_message foreign key (sender) references users(id) on delete cascade,
    constraint to_user_message foreign key (recipient) references users(id) on delete cascade
);

create table tags
(
    id integer generated always as identity primary key,
    name text not null,
    constraint unique_tag unique(name)
);

create table languages
(
    id integer generated always as identity primary key,
    name text not null
);

create table blocks
(
    id integer generated always as identity primary key,
    blocker integer not null,
    blocked integer not null,
    constraint blocker_user foreign key (blocker) references users(id) on delete cascade,
    constraint blocked_user foreign key (blocked) references users(id) on delete cascade
);

create table tags_users
(
    id integer generated always as identity primary key,
    creator integer not null,
    tag integer not null,
    constraint tags_user_user foreign key (creator) references users(id) on delete cascade,
    constraint tag_user_tag foreign key (tag) references tags(id)
);

create table languages_users
(
    id integer generated always as identity primary key,
    speaker integer not null,
    language integer not null,
    constraint languages_user_user foreign key (speaker) references users(id) on delete cascade,
    constraint language_user_language foreign key (language) references languages(id),
    constraint unique_user_language unique(speaker, language)
);

create table recoveries
(
    id integer generated always as identity primary key,
    uuid char(36) not null,
    created timestamptz default (utc_now()),
    active boolean default true,
    recoverer integer not null,
    constraint recoverer foreign key (recoverer) references users(id) on delete cascade
);

create table karma_histories
(
    id integer generated always as identity primary key,
    target integer not null,
    amount integer not null,
    date timestamptz not null default (utc_now()),

    constraint target_karma_history foreign key (target) references users(id) on delete cascade
);

--to be run with pg cron
create or replace function crunch_karma_history(hours_time integer)
    returns void as
$$
begin
    create temporary table temp_karmas (id integer, target integer, amount integer ) on commit drop;
    insert into temp_karmas
    select id,
            target,
            amount
    from karma_histories
    where extract(epoch from (utc_now()) - date ) / 3600 <= hours_time;
    delete from karma_histories k
    where exists(select t.id from temp_karmas t where t.id = k.id);
    insert into karma_histories(target, amount)
    select t.target,
            sum(t.amount)
    from temp_karmas t
    group by t.target;
end;
$$
language plpgsql;

-- select cron.schedule('0 * * * *', $$select crunch_karma_history(1)$$);
-- select cron.schedule('10 0 * * *', $$select crunch_karma_history(24)$$);
-- select cron.schedule('30 0 * * 1', $$select crunch_karma_history(24 * 7)$$);

create table karma_leaderboard
(
    id integer generated always as identity primary key,
    ranker integer not null,
    position integer not null,
    current_karma integer not null,
    gained integer not null,
    date timestamptz not null default (utc_now()),

    constraint ranker_user foreign key (ranker) references users(id) on delete cascade
);

--to be run with pg cron
create or replace function compute_leaderboard()
    returns void as
$$
begin
    create temporary table temp_leaderboard (ranker integer, current_karma integer, position integer, gained integer) on commit drop;
    insert into temp_leaderboard(ranker, current_karma, position, gained)
    select  target,
            total,
            row_number() over (order by total desc),
            (select total - coalesce((select k.current_karma from karma_leaderboard k where k.ranker = target),0) )
    from (select target, sum(amount) total from karma_histories group by target) h;
    truncate table karma_leaderboard;
    insert into karma_leaderboard(ranker, current_karma, position, gained)
    select * from temp_leaderboard;
end;
$$
language plpgsql;

-- select cron.schedule('0 */4 * * *', $$select crunch_karma_history()$$);

create table histories
(
    id integer generated always as identity primary key,
    sender integer not null,
    recipient integer not null,
    first_message_date timestamptz not null default (utc_now()),
    last_message_date timestamptz not null default (utc_now()),
    sender_archived boolean not null default false,
    recipient_archived boolean not null default false,
    sender_deleted_to int,
    recipient_deleted_to int,

    constraint from_user_message foreign key (sender) references users(id) on delete cascade,
    constraint to_user_message foreign key (recipient) references users(id) on delete cascade,

    unique(sender, recipient)
);

create table suggestions
(
    id integer generated always as identity primary key,
    suggested integer not null,
    score integer not null,

    constraint user_suggested foreign key (suggested) references users(id) on delete cascade
);

create table reports
(
    id integer generated always as identity primary key,
    comment text,
    reason smallint not null,
    reporter integer not null,
    date timestamptz not null default (utc_now()),
    reported integer not null,
    constraint reporter_user foreign key (reporter) references users(id) on delete cascade,
    constraint reported_user foreign key (reported) references users(id) on delete cascade
);

create table stock_text
(
    id integer generated always as identity primary key,
    contents text not null unique,
    text_type smallint not null
);

-- create table badges
-- (
--     id integer generated always as identity primary key,
--     kind smallint not null,
--     description text
-- );

-- create table reactions
-- (
--     id integer generated always as identity primary key,
--     reward integer not null,
--     kind smallint not null,
--     description text
-- );

-- create table privileges
-- (
--     id integer generated always as identity primary key,
--     feature smallint not null,
--     description text,
--     quantity integer not null
-- );

-- create table privileges_users
-- (
--     id integer generated always as identity primary key,
--     privilege integer not null,
--     receiver integer not null,
--     constraint privilege_user_user foreign key (receiver) references users(id) on delete cascade,
--     constraint privilege_user_privilege foreign key (privilege) references privileges(id) on delete cascade
-- );

-- create table badges_users
-- (
--     id integer generated always as identity primary key,
--     receiver integer not null,
--     badge integer not null,
--     constraint badge_user_user foreign key (receiver) references users(id) on delete cascade,
--     constraint badge_user_badge foreign key (badge) references badges(id)
-- );

-- create table reactions_messages
-- (
--     id integer generated always as identity primary key,
--     reaction integer not null,
--     message bigint not null,
--     constraint reaction_message_message foreign key (message) references messages(id) on delete cascade,
--     constraint reaction_message_reaction foreign key (reaction) references reactions(id) on delete cascade
-- );

-- create table reactions_users
-- (
--     id integer generated always as identity primary key,
--     bearer integer not null,
--     reaction integer not null,
--     constraint reaction_user_user foreign key (bearer) references users(id) on delete cascade,
--     constraint reaction_user_reaction foreign key (reaction) references reactions(id) on delete cascade
-- );

create table experiments
(
    id integer generated always as identity primary key,
    code integer not null,
    name text not null,
    description text not null,
    added timestamptz not null default (utc_now())
);

create or replace function insert_history
(sender_id int, recipient_id int) returns void as
$$
begin
    if exists(select 1 from histories where sender = recipient_id and recipient = sender_id) then
        update histories set sender_archived = false, recipient_archived = false, last_message_date = (utc_now()) where sender = recipient_id and recipient = sender_id;
    else
        insert into histories (sender, recipient)
        values (sender_id, recipient_id)
        on conflict (sender, recipient) do
        update set sender_archived = false, recipient_archived = false, last_message_date = (utc_now());
    end if;
end;
  $$
  language plpgsql;

create or replace function truncate_tables()
  returns void as
$body$
begin
        truncate table users  restart identity cascade;
        truncate table messages restart identity cascade ;
        truncate table tags restart identity cascade ;
end;
  $body$
  language plpgsql;

create or replace function date_part_age(part text, tm timestamptz)
  returns integer as
$body$
begin
    return date_part(part, age(utc_now(), tm));
end;
  $body$
  language plpgsql;

insert into languages
    (name)
values
    ('Albanian'),
    ('Amharic'),
    ('Arabic'),
    ('Arapaho'),
    ('Armenian'),
    ('Awadhi'),
    ('Azerbaijani'),
    ('Basque'),
    ('Bengali'),
    ('Bhojpuri'),
    ('Bosnian'),
    ('Breton'),
    ('Burmese'),
    ('Bulgarian'),
    ('Cebuano'),
    ('Cantonese'),
    ('Catalan'),
    ('Cornish'),
    ('Croatian'),
    ('Czech'),
    ('Danish'),
    ('Dutch'),
    ('English'),
    ('Esperanto'),
    ('Estonian'),
    ('Faroese'),
    ('Finnish'),
    ('French'),
    ('Galician'),
    ('German'),
    ('Georgian'),
    ('Greek'),
    ('Guarani'),
    ('Gujarati'),
    ('Haitian Creole'),
    ('Hausa'),
    ('Hebrew'),
    ('Hindi'),
    ('Hmong'),
    ('Hungarian'),
    ('Icelandic'),
    ('Igbo'),
    ('Indonesian'),
    ('Italian'),
    ('Irish Gaelic'),
    ('Japanese'),
    ('Javanese'),
    ('Kannada'),
    ('Khmer'),
    ('Klingon'),
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
    ('Mandarin'),
    ('Manx'),
    ('Marathi'),
    ('Mongolian'),
    ('Mongul'),
    ('Navajo'),
    ('Norwegian'),
    ('Ojibwe'),
    ('Oriya'),
    ('Panjabi'),
    ('Papiamentu'),
    ('Pashto'),
    ('Persian'),
    ('Polish'),
    ('Portuguese'),
    ('Romanian'),
    ('Russian'),
    ('Sanskrit'),
    ('Sauk'),
    ('Scots Gaelic'),
    ('Serbian'),
    ('Sindhi'),
    ('Sioux'),
    ('Slovak'),
    ('Slovenian'),
    ('Spanish'),
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
    ('Waray'),
    ('Welsh'),
    ('Wolof'),
    ('Yiddish'),
    ('Yoruba'),
    ('Yuchi'),
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
    ('Bahamas'),
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
    ('Congo'),
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
    ('Gambia'),
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
    ('San Marino'),
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

insert into experiments (code, name, description) values (0, 'Impersonation', 'Temporarily change your profile to a character, famous person or historical figure so you can chat as if it was the same person typing it');

insert into stock_text (contents, text_type) values
    ('I stayed up all night wondering where the sun went, then it dawned on me', 0),
    ('The problem with trouble shooting is that trouble shoots back', 0),
    ('Here''s a picture of me with REM. That''s me in the corner', 0),
    ('I bought myself some glasses. My observational comedy improved', 0),
    ('If you arrive fashionably late in Crocs, you''re just late', 0),
    ('Do Transformers get car or life insurance', 0),
    ('I can''t believe I got fired from the calendar factory. All I did was take a day off', 0),
    ('Money talks. Mine always says goodbye', 0),
    ('Why do bees hum? They don''t remember the lyrics', 0),
    ('Most people are shocked when they find out how bad I am as an electrician', 0),
    ('There''s a new restaurant called Karma. There''s no menu. You get what you deserve', 0),
    ('Thanks for explaining the word "many" to me, it means a lot', 0),
    ('I''m reading a book about anti-gravity. It''s impossible to put down', 0),
    ('I wasn''t originally going to get a brain transplant, but then I changed my mind', 0),
    ('R.I.P boiled water. You will be mist', 0),
    ('What is the best thing about living in Switzerland? Well, the flag is a big plus', 0),
    ('Atheism is a non-prophet organization', 0),
    ('The future, the present, and the past walked into a bar. Things got a little tense', 0),
    ('At what age is it appropriate to tell my dog that he''s adopted', 0),
    ('I started out with nothing, and I still have most of it', 0),
    ('Did Noah include termites on the ark', 0),
    ('The man who created autocorrect has died. Restaurant in peace', 0),
    ('I used to think I was indecisive, but now I''m not too sure', 0),
    ('The first time I got a universal remote control, I thought to myself, "This changes everything"', 0),
    ('I recently decided to sell my vacuum cleaner - all it was doing was gathering dust', 0),
    ('Where there''s a will, there''s a relative', 0),
    ('I''m skeptical of anyone who tells me they do yoga every day. That''s a bit of a stretch', 0),
    ('When dogs go to sleep, they read bite-time stories before bed', 0),
    ('Dogs hate driving because they can never find a barking space', 0),
    ('People who use selfie sticks really need to have a good, long look at themselves', 0),
    ('A thesaurus is great. There''s no other word for it', 0),
    ('I saw a documentary on how ships are kept together. Riveting', 0),
    ('People who like trance music are very persistent. They don''t techno for an answer', 0),
    ('Do Transformers get car or life insurance', 0),
    ('I once saw two people wrapped in a barcode and had to ask, Are you an item?', 0),
    ('I went to buy camouflage trousers, but I couldn''t find any', 0),
    ('I, for one, like Roman numerals', 0),
    ('My partner told me to stop impersonating a flamingo. I had to put my foot down', 0),
    ('Keep the dream alive - hit your snooze button', 0),
    ('It sure takes a lot of balls to golf the way I do', 0),
    ('I was wondering why the ball kept getting bigger and bigger, and then it hit me', 0),
    ('With the rise of self-driving vehicles, it''s only a matter of time before we get a country song where a guy''s truck leaves him too', 0),
    ('The person who invented knock-knock jokes should get a no bell prize', 0),
    ('The other day I asked the banker to check my balance, so they pushed me', 0),
    ('A baseball walks into a bar, and the bartender throws it out', 0),
    ('That awkward moment when you leave a store without buying anything and all you can think is "act natural, you''re innocent"', 0),
    ('Never trust atoms; they make up everything', 0),
    ('If a parsley farmer gets sued, can they garnish his wages', 0),
    ('I didn''t think orthopedic shoes would help, but I stand corrected', 0),
    ('It was an emotional wedding. Even the cake was in tiers', 0),
    ('I just got kicked out of a secret cooking society. I spilled the beans', 0),
    ('How does the man in the moon get his hair cut? Eclipse it', 0),
    ('Why did Beethoven get rid of his chickens? All they said was, "Bach, Bach, Bach..."', 0),
    ('What do you call a mobster who''s buried in cement? A hardened criminal', 0),
    ('Knock, knock. Who/s there? Nobel. Nobel who? Nobel, so I knock knocked', 0),
    ('What did the zookeeper say after the python broke free? Nothing', 0),
    ('Two men walk into a bar. You''d think at least one of them would have ducked', 0),
    ('What do you call a guy who''s had too much to drink? A cab', 0),
    ('What''s a frog''s favorite type of shoes? Open toad sandals', 0),
    ('Blunt pencils are really pointless', 0),
    ('6:30 is the best time on a clock, hands down', 0),
    ('Well, to be Frank with you, I''d have to change my name', 0),
    ('What if there were no hypothetical questions', 0),
    ('Are people born with photographic memories, or does it take time to develop', 0),
    ('A book fell on my head the other day. I only have my shelf to blame though', 0),
    ('My friend''s bakery burned down last night. Now his business is toast', 0),
    ('I doubt, therefore, I might be', 0),
    ('I used to have a handle on life, but then it broke', 0),
    ('When everything is coming your way - you''re in the wrong lane', 0),
    ('A termite walks into the bar and asks, "Is the bar tender here?', 0),
    ('Always borrow money from a pessimist; they''ll never expect it back', 0),
    ('Don''t you hate it when someone answers their own questions? I do', 0),
    ('What happens to a frog''s car when it breaks down? It gets toad away', 0),
    ('Why did the burglar rob a bakery? He needed the dough', 0),
    ('I put my grandma on speed dial. I call that Instagram', 0),
    ('Why did the scarecrow win an award? He was outstanding in his field', 0),
    ('Whiteboards are remarkable', 0),
    ('Will glass coffins be a success? Remains to be seen', 0),
    ('Geology rocks, but geography''s where it''s at', 0),
    ('The rotation of Earth really makes my day', 0),
    ('So what if I don''t know what "Armageddon" means? It''s not the end of the world', 0),
    ('People who take care of chickens are literally chicken tenders', 0),
    ('Despite the high cost of living, it remains popular', 0),
    ('I bought the world''s worst thesaurus yesterday. Not only is it terrible, but it''s also terrible', 0),
    ('What goes, "Oh, oh, oh"? Santa walking backward', 0),
    ('What do you call a sleeping dinosaur? A dino-snore', 0),
    ('Why did the picture go to jail? Because it was framed', 0),
    ('For maximum attention, nothing beats a good mistake', 0),
    ('The older I get, the earlier it gets late', 0),
    ('Photons have mass? I didn''t even know they were Catholic', 0),
    ('The dinner I was cooking for my family was going to be a surprise but the fire trucks ruined it', 0),
    ('To the mathematicians who thought of the idea of zero, thanks for nothing!', 0),
    ('The adjective for metal is metallic, but not so for iron which is ironic', 0),
    ('Being an adult is just walking around wondering what you''re forgetting', 0),
    ('Why do the French eat snails? They don''t like fast food', 0),
    ('We can''t help everyone, but everyone can help someone', 0),
    ('Improve your memory by doing unforgettable things', 0),
    ('I hate people who use big words just to make themselves look perspicacious', 0),
    ('My boss told me to have a good day. So I went home', 0),
    ('Anyone who has never made a mistake has never tried anything new', 0);
