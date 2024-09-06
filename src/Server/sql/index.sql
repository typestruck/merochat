create or replace function utc_now()
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
    password text,
    joined timestamptz default (utc_now()),
    email text,
    birthday date,
    gender smallint,
    headline text not null,
    avatar text,
    backer boolean not null default false,
    description text not null,
    country integer,
    receive_email smallint not null default(0),
    visibility smallint not null default 0,
    visibility_last_updated timestamptz default (utc_now()),
    read_receipts boolean not null default true,
    typing_status boolean not null default true,
    online_status boolean not null default true,
    message_timestamps boolean not null default true,
    completed_tutorial boolean not null default false,
    temporary boolean not null default false,

    constraint country_user foreign key (country) references countries(id)
);

create table messages
(
    id integer generated always as identity primary key,
    sender integer not null,
    recipient integer not null,
    date timestamptz not null default (utc_now()),
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
    constraint tag_user_tag foreign key (tag) references tags(id) on delete cascade
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

create table feedbacks (
    id integer generated always as identity primary key,
    comments text not null,
    file_name text,
    feedbacker integer not null,
    constraint feedback_user foreign key (feedbacker) references users(id) on delete cascade
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

-- select cron.schedule('0 * * * *', $$select crunch_karma_history(1)$$);
-- select cron.schedule('10 0 * * *', $$select crunch_karma_history(24)$$);
-- select cron.schedule('20 0 * * 1', $$select crunch_karma_history(24 * 7)$$);
-- select cron.schedule('30 4 1 * 4', $$select crunch_karma_history(24 * 7 * 4)$$);
-- select cron.schedule('45 9 1 8 3', $$select crunch_karma_history(24 * 7 * 4 * 12)$$);
create or replace function crunch_karma_history(hours_time integer)
    returns void as
$$
begin
    create temporary table temp_karmas (id integer, target integer, amount integer) on commit drop;
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

create table karma_leaderboard
(
    id integer generated always as identity primary key,
    ranker integer not null unique,
    position integer not null,
    current_karma integer not null,
    gained integer not null,
    date timestamptz not null default (utc_now()),

    constraint ranker_user foreign key (ranker) references users(id) on delete cascade
);

-- select cron.schedule('0 */4 * * *', $$select compute_leaderboard()$$);
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

-- select cron.schedule('45 2 * * *', $$select purge_temporary_users()$$);
create or replace function purge_temporary_users()
    returns void as
$$
begin
    delete from users u where exists(select 3 from users where u.id = id and temporary and extract(epoch from (utc_now()) - joined ) / 3600 > 84.0);
end;
$$
language plpgsql;

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

create table last_seen (
    id integer generated always as identity primary key,
    who integer not null unique,
    date timestamptz not null
);

create table tokens (
    id integer generated always as identity primary key,
    toker integer not null,
    contents text not null unique,
    constraint tokens_user_user foreign key (toker) references users(id) on delete cascade
);

create table badges
(
    id integer generated always as identity primary key,
    kind smallint not null,
    description text
);

create table badges_users
(
    id integer generated always as identity primary key,
    receiver integer not null,
    badge integer not null,
    constraint badge_user_user foreign key (receiver) references users(id) on delete cascade,
    constraint badge_user_badge foreign key (badge) references badges(id) on delete cascade
);

-- create table reactions
-- (
--     id integer generated always as identity primary key,
--     reward integer not null,
--     kind smallint not null,
--     description text
-- );

create table privileges
(
    id integer generated always as identity primary key,
    feature smallint not null,
    name text,
    description text,
    quantity integer not null
);

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

create table unsubscribe_tokens
(
    id integer generated always as identity primary key,
    unsubscriber integer not null,
    contents text not null,
    constraint unsubcribe_tokens_user foreign key (unsubscriber) references users(id) on delete cascade
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

create index sender_recipient_messages on messages(sender, recipient);

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

insert into experiments (code, name, description) values
    (0, 'Impersonation', 'Temporarily change your profile to a character, famous person or historical figure so you can chat as if it was the same person typing it');

insert into stock_text (contents, text_type) values
    ('I stayed up all night wondering where the sun went, then it dawned on me', 0),
    ('The problem with trouble shooting is that trouble shoots back', 0),
    ('Here''s a picture of me with REM. That''s me in the corner', 0),
    ('I bought myself some glasses. My observational comedy improved', 0),
    ('If you arrive fashionably late in Crocs, you''re just late', 0),
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
    ('Anyone who has never made a mistake has never tried anything new', 0),
    ('I''m not superstitious, but I am a little stitious', 1),
    ('I walk around like everything''s fine, but deep down, inside my shoe, my sock is sliding off', 1),
    ('There is no sunrise so beautiful that it is worth waking me up to see it', 1),
    ('If I''m not back in five minutes, just wait longer', 1),
    ('Never put off till tomorrow what you can do the day after tomorrow just as well', 1),
    ('I can carry on a conversation made up entirely of movie quotes', 1),
    ('Able to do mind reading', 1),
    ('I generally avoid temptation unless I can''t resist it', 1),
    ('Would I rather be feared or loved? Easy. Both. I want people to be afraid of how much they love me', 1),
    ('All I''ve ever wanted was an honest week''s pay for an honest day''s work', 1),
    ('If you can''t be kind, at least be vague', 1),
    ('Sometimes you lie in bed at night and you don''t have a single thing to worry about. That always worries me', 1),
    ('Between two evils, I always pick the one I never tried before', 1),
    ('It''s amazing that the amount of news that happens in the world every day always just exactly fits the newspaper', 1),
    ('I believe that if life gives you lemons, you should make lemonade... And try to find somebody whose life has given them vodka, and have a party', 1),
    ('I never said most of the things I said', 1),
    ('People who think they know everything are a great annoyance to those of us who do', 1),
    ('I enjoy urban planning', 1),
    ('I know the exact location of every food item in the supermarket', 1),
    ('The laws of physics do not apply to me', 1),
    ('I am so clever that sometimes I don''t understand a single word of what I am saying', 1),
    ('A monad is just a monoid in the category of endofunctors, what''s the problem?', 1),
    ('More than half of your bones are located in the hands, wrists, feet, and ankles', 1),
    ('Did you know the unicorn is the national animal of Scotland?', 1),
    ('Fun fact: nutmeg is a hallucinogen', 1),
    ('Fun fact: you can hear a blue whale''s heartbeat from over 2 miles away', 1),
    ('Remember, humans are the only animals that blush', 1),
    ('Try it: you can''t hum if you hold your nose', 1),
    ('I have tried 187 of the 200 different Japanese Kit Kats flavours', 1),
    ('Hawaiian pizza was created in Ontario, Canada, by Greek immigrant Sam Panopoulos in 1962', 1),
    ('Proud to pronounce “Taumatawhakatangihangakoauauotamateaturipukakapikimaungahoronukupokaiwhenuakitanatahu” instead of Taumata Hill', 1),
    ('How many of Sweden''s 267,570 islands have you been to?', 1),
    ('Friendly reminder: vending machines kill more people than sharks', 1),
    ('Back in my day, carrots were still purple', 1),
    ('My chilhood nickname was "sixth sick sheik''s sixth sheep''s sick"', 1),
    ('What''s the most interesting thing you''ve read lately?', 1),
    ('If you were in charge of the playlist, which song would you play next?', 1),
    ('What''s the best gift you''ve ever gotten?', 1),
    ('Are there any common misconceptions about your job?', 1),
    ('If you were giving a presentation, what would the topic be?', 1),
    ('If you could only eat one thing for the rest of your life, what would you choose?', 1),
    ('What is your favorite guilty pleasure TV show?', 1),
    ('Are you who you thought you''d be as a kid?', 1),
    ('What are the top three things on your bucket list?', 1),
    ('What is one thing you wish you could do that you know you probably never will?', 1),
    ('What''s the first thing you would do if you won the lottery?', 1),
    ('Do you ever sing in the shower?', 1),
    ('Tell me the funniest joke you''ve ever heard', 1),
    ('What is something that''s really popular right now that will be ridiculous in five years?', 1),
    ('What''s your favorite movie that you could watch over and over again?', 1),
    ('What do you think the world will be like 50 years in the future?', 1),
    ('What did you want to be when you were young? ', 2),
    ('If you found a genie, what three wishes would you ask for?', 2),
    ('If money weren''t an issue, what would you do?', 2),
    ('Do you consider yourself a lemon or a lime? Why?', 2),
    ('What would be your DJ name?', 2),
    ('What is your superpower?', 2),
    ('What is the most random fact you know?', 2),
    ('Would you rather spend a weekend in a tropical island or a snowy mountain?', 2),
    ('Would you rather always be two hours early or 20 minutes late?', 2),
    ('Would you rather be a water bear or a blue whale?', 2),
    ('What subject do you wish was taught in every school?', 2),
    ('If you could know the answer to any question, what would it be?', 2),
    ('What would you do if you knew you couldn''t fail?', 2),
    ('If you could be any fictional character, who would you be?', 2),
    ('If you had to invent a new ice cream, what would it be?', 2),
    ('What would be the title of your biography?', 2),
    ('What three items would you bring with you on a deserted island?', 2),
    ('If you could live in a different country for a year, which country would you choose?', 2),
    ('Does any song brings you back childhood memories?', 2),
    ('Which band would you join? And what would your role be?', 2),
    ('If you could name a band, what would it be called?', 2),
    ('How has your taste in music changed in the past 10 years?', 2),
    ('What is your go-to karaoke song?', 2),
    ('If you were a genre of music, what would it be?', 2),
    ('If you could add a word to the dictionary what would you add and what would it mean?', 2),
    ('If you could meet any person or character for dinner, who would you pick and why?', 2),
    ('What is your most used emoji?', 2),
    ('If you had 25 hours a day, how would you spend your extra time?', 2),
    ('If you had to eat one meal every day for the rest of your life what would it be?', 2),
    ('What''s the weirdest food you''ve ever eaten?', 2),
    ('What are your three favorite foods?', 2),
    ('What is something you are great at cooking?', 2),
    ('What is something you can''t cook?', 2),
    ('If you could make everyone forget one thing, what would it be?', 2),
    ('The zombie apocalypse is coming, who are 3 people you want on your team?', 2),
    ('Have you ever been told you look like someone famous, and who was it?', 2),
    ('If you were famous, what would you be famous for?', 2),
    ('What was your least favorite food as a child? Do you still hate it or do you love it now?', 2),
    ('If you were left on a deserted island with either your worst enemy or no one, which would you choose?', 2),
    ('If aliens landed on earth tomorrow and offered to take you home with them, would you go?', 2),
    ('What''s your favorite dinosaur?', 2),
    ('If you had a superpower but could only use it for mundane tasks, like washing the dishes or folding the laundry, what would it be?', 2),
    ('If you could bring any inanimate object to life as a pet, what would you choose?', 2),
    ('If you could create any holiday, with decorations and traditions, what would it be called?', 2),
    ('If you weren''t here, what would you be doing?', 2),
    ('What is your favorite holiday?', 2),
    ('What''s your biggest pet peeve?', 2),
    ('What is the thing that annoys you most in life?', 2);

insert into privileges (feature, name, description, quantity) values
    (0, 'Receive chats', 'Access basic chat features, edit your profile and settings', 1),
    (100, 'Start chats', 'View new chat suggetions, and start new chats', 25),
    (200, 'Participate in chat experiments', 'Take part in chat experiments. Some experiments may require more karma', 50),
    (201, 'Impersonation', 'Start Impersonation chat experiment', 1000),
    (300, 'More tags', 'Increased number of max profile tags', 250),
    (400, 'Send links', 'Send markdown links', 1000),
    (500, 'Send images', 'Send pictures in chats', 3000);

insert into badges (kind, description) values
    (0, 'Admin' ),
    (100, 'Contributor');
