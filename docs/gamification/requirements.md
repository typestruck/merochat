# Gamification

Game like features of the application.

## Table of contents

1. [Karma](#karma)

2. [Reactions](#reactions)

3. [Trophies](#trophies)

4. [Badges](#badges)

5. [Chat Gimmicks](#chat-gimmicks)

## Karma

Karma is ~fake internet points~ a score of how much MelanChat trust an User. Higher numbers are better. Karma is primary gained by chatting, and also by collecting [trophies](#trophies) or [reactions](#reactions).

A new User starts off with the Karma earned by new User [trophies](#trophies).

**CONSTANT** _in this document is equal to 10_

* Karma algorithm ([wheel](https://github.com/melanchat/melanchat/blob/master/src/Server/Wheel.purs))

    todo: copy linked file here

* Karma rank

    * Position of an User in the relation to all others in number of Karma points; Users can see the Karma points of 3 users above them and all beneath

    * This is purely for show

* Karma privileges

    How much Karma points is needed to unlock features on the application:

    1. Chatting

        * Any amount of Karma

    2. Leave reactions on individual messages

        * CONSTANT Karma * 2

    3. Update user typing (see [Settings](../user/requirements.md#update-settings) and [Chat Input](../im/requirements.md#chat-input))

        * CONSTANT Karma * 2

    4. Update message time (see [Settings](../user/requirements.md#update-settings) and [Chat History](../im/requirements.md#chat-history))

        * CONSTANT Karma * 3

    5. Participate in chat gimmicks

        * CONSTANT Karma * 4

    6. Update last message for contact (see [Settings](../user/requirements.md#update-settings) and [Contact List](../im/requirements.md#contact-list))

        * CONSTANT Karma * 5

    7. Update messages status (see [Settings](../user/requirements.md#update-settings) and [Chat History]7(../im/requirements.md#chat-history))

        * CONSTANT Karma * 6

    8. Update online status (see [Settings](../user/requirements.md#update-settings) and [Chat History](../im/requirements.md#chat-history))

        * CONSTANT Karma * 7

    9. Leave reactions on Users

        * CONSTANT Karma * 10

    10. Go back in suggestions

    11. Send audio messages

        * CONSTANT Karma * 20

    12. Send pictures

        * CONSTANT Karma * 25

    13. Send videos

        * CONSTANT Karma * 30

    14. Send video messages

        * CONSTANT Karma * 50

    15. Participate in moderation

        * CONSTANT Karma * 250

* Losing Karma

## Reactions

Reactions are emoji like avaliations left for Users or individual messages.

* Loving

    * Heart emoji

    * CONSTANT Karma for User reaction or 1 Karma for individual message reaction

* Angry

    * Only available for individual messages reactions

    * Angry face emoji

* Surprised

    * Surprised face emoji

    * CONSTANT / 2 Karma for User reaction or 1 Karma for individual message reaction

* Sad

    * Only available for individual messages reactions

    * Sad face emoji

* Amused

    * Happy face emoji

    * CONSTANT Karma for User reaction or 1 Karma for individual message reaction

* Smirking

    * Only available for individual messages reactions

    * Happy face emoji

## Trophies

Trophies are achievements excluding Karma milestones. Different from badges, trophies earn Karma.

* New user

    * Earned by registering a permanent account

    * CONSTANT Karma

* Completed tutorial

    * CONSTANT Karma

* First chat

    * Earned when an User is replied in a chat for the first time ever

    * CONSTANT * 2 Karma

* Login streak

    * Counted every 24 hours of being logged in

    * 1 Karma every per day

* Anniversary

    * Counted yearly (duh)

    * CONSTANT Karma per month on the site with 1 replied chat in that year

* Number of messages

    * Counted at 50, 100, 300, 500 and then by thousands of messages visualized (i.e. a trophy at 1000 messages, another at 2000 etc)

    * CONSTANT * (messages visualized / 1000) Karma

* Number of chats started

    * Counted at 10, 30, 50 and then by hundreds of chats replied (i.e. a trophy at 100 chats, another at 200 etc)

    * CONSTANT * (chats replied / 100) Karma

* Number of chats received

    * Counted at 10, 30, 50 and then by hundreds of chats received (i.e. a trophy at 100 chats, another at 200 etc)

    * CONSTANT * (chats received / 100) Karma

* Number of reactions

    TBD

## Badges

Badges are Karma milestone achievements. Different from trophies, badges do not earn Karma.

* Rookie

    * CONSTANT * 10 Karma

* Learning

    * CONSTANT * 25 Karma

* Professional

    * CONSTANT * 75 Karma

* Experienced

    * CONSTANT * 150 Karma

* Expert

    * CONSTANT * 300 Karma

* Master

    * CONSTANT * 450 Karma

* Grandmaster

    * CONSTANT * 550 Karma

* Hero

    * CONSTANT * 750 Karma

* Legend

    * CONSTANT * 1000 Karma

* Demigod

    * CONSTANT * 5000 Karma

* God

    * CONSTANT * 25000 Karma

* Galaxy Brain

    * CONSTANT * 100000 Karma

## Chat Gimmicks

Special chat features selected for random Users to take part in.

* User can see their chat partner message as it is typed

* Group chat in which the size of participants double every x time

* Find patient zero

* Debate mode

* Chat as x historical figure/character/famous person
