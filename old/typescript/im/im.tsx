import * as melan from '../melanchat'
import React, { MouseEvent, Component, RefObject } from 'react'
import ReactDOM from 'react-dom'
import { User, change } from '../types'
import * as comp from '../component'
import Chat from './chat'
import { BrowserHistoryData, Filter, ContactListState, MessageEventStatus, ImProps, ContactListProps, ProfileProps, MessageContent, ProfileState, ImState, SettingsProps, UserHistory, MessageStatus, Message, SettingsState } from './types'

/** Settings show basic user information, a link to user profile and settings and a button to logout. */
class Settings extends Component<SettingsProps, SettingsState> {

    menuDiv: RefObject<HTMLDivElement> = React.createRef();

    state = {
        menuClass: ''
    };

    componentDidMount() {
        document.addEventListener('click', event => {
            if (!this.menuDiv.current.contains(event.target as Node))
                this.setState({ menuClass: '' });
        });
    }

    render() {
        return (
            <div id="settings" className="settings">
                <a href="/settings/profile"><img className="avatar-settings" src={this.props.user.avatar} /></a>
                <div className="settings-name">
                    <strong>{this.props.user.name}</strong><br />
                    Karma: 9001 (2) (1) (5)
                </div>
                <div ref={this.menuDiv} className={`menu-button outer-drop-menu ${this.state.menuClass}`}>
                    <a className="menu-button" onClick={this.showMenu}>
                        {/* USE FLEX-GROW */}
                        <svg className="svg-right i-ellipsis-vertical svg-32 svg-more" viewBox="0 0 32 32">
                            <circle cx="16" cy="7" r="2"></circle>
                            <circle cx="16" cy="16" r="2"></circle>
                            <circle cx="16" cy="25" r="2"></circle>
                        </svg>
                    </a>
                    <div className="drop-menu fade-in effect">
                        <a className="menu-button" href="/settings/profile">Profile</a>
                        <a className="menu-button" href="/settings">Settings</a>
                        <i>🍉</i>
                        <a href="#">Help</a>
                        <a className="menu-button" href="#">Become a backer</a>
                        <i>🍉</i>
                        <a className="menu-button" onClick={melan.logout}>Logout</a>
                    </div>
                </div>
            </div>
        );
    }

    showMenu = event => {
        event.stopPropagation();

        this.setState((state, _) => ({ menuClass: state.menuClass ? '' : 'dropdown-wrapper-visible' }));
    }
}

//TODO: order by unread, date
/** Displays the users this person has chatted with. */
class ContactList extends Component<ContactListProps, ContactListState> {

    filterOptionsDiv: RefObject<HTMLDivElement> = React.createRef();

    changer: change<ContactListState> = comp.changer.bind(this);

    state = {
        filterOptionsClass: '',
        chatOptionsClass: [],
        searchQuery: '',
        filter: Filter.Active
    };

    componentDidMount() {
        document.addEventListener('click', event => {
            let chatOptions = document.querySelector('.menu-button.chat-options.dropdown-wrapper-visible');

            if (!this.filterOptionsDiv.current.contains(event.target as Node))
                this.setState({ filterOptionsClass: '' });
            if (chatOptions && !chatOptions.contains(event.target as Node))
                this.setState({ chatOptionsClass: [] });
        });

        window.addEventListener('popstate', event => {
            let state = event.state as BrowserHistoryData;

            if (state && state.filter)
                this.setState({ filter: state.filter });
        });
    }

    render() {
        return (<>
            <div className="search">
                <svg className="i-search svg-16 search-icon" viewBox="0 0 32 32">
                    <circle cx="14" cy="14" r="12"></circle>
                    <path d="M23 23 L30 30"></path>
                </svg>
                <input type="text" placeholder="Search your chats" onChange={this.changer('searchQuery')} />
                <div ref={this.filterOptionsDiv} className={`menu-button search-options ${this.state.filterOptionsClass}`}>
                    {/* USE FLEX-GROW */}
                    <a className="menu-button" onClick={this.showMenu('filterOptionsClass')}>
                        <svg className="i-options svg-32 svg-right" viewBox="0 0 32 32">
                            <path d="M28 6 L4 6 M28 16 L4 16 M28 26 L4 26 M24 3 L24 9 M8 13 L8 19 M20 23 L20 29"></path>
                        </svg>
                    </a>
                    <div className="drop-menu fade-in effect">
                        <a className="menu-button" onClick={this.filterChats(Filter.All)}>All</a>
                        <a className="menu-button" onClick={this.filterChats(Filter.Active)}>Active</a>
                        <a className="menu-button" onClick={this.filterChats(Filter.Unread)}>Unread</a>
                    </div>
                </div>
            </div>
            <div className="contact-list">
                {this.props.contacts.map((c, i) => {
                    if (c.user.name.toLowerCase().includes(this.state.searchQuery.toLocaleLowerCase()) && this.matchFilters(c)) {
                        let classes = 'contact',
                            unread = this.fromLastSent(c.history).filter(h => isUnread(h, this.props.user)).length || '';

                        if (this.props.chatting && c.user.id == this.props.chatting.user.id)
                            classes = `${classes} chatting`;

                        return (
                            <div className={classes} key={c.user.id} onClick={() => this.setChatting(c)}>
                                <img className="avatar-contact-list" src={c.user.avatar} />
                                <div className="contact-profile">
                                    <strong>{c.user.name}</strong><br />
                                    <i className="contact-list-description">{c.user.headline}</i>
                                </div>
                                <div className={`menu-button chat-options ${this.state.chatOptionsClass[i] || ''}`}>
                                    {unread}
                                    <a className="menu-button" onClick={this.showMenu('chatOptionsClass', i)}>
                                        <svg className="i-chevron-bottom svg-16 svg-right" viewBox="0 0 32 32">
                                            <path d="M30 12 L16 24 2 12"></path>
                                        </svg>
                                    </a>
                                    <div className="drop-menu fade-in effect">
                                        {!c.archived && <a className="menu-button" onClick={this.archiveChat(c.user.id)}>Archive</a>}
                                        {!unread && <a className="menu-button" onClick={this.markAsUnread(c.user.id)}>Mark as unread</a>}
                                        {unread && <a className="menu-button" onClick={this.markAsRead(c.user.id)}>Mark as read</a>}
                                    </div>
                                </div>
                            </div>
                        );
                    }
                })}
            </div>
        </>);
    }

    fromLastSent(history: Message[]) {
        let lastIndex;

        for (let i = history.length - 1; i--; i >= 0)
            if (history[i].sender == this.props.user.id)
                lastIndex = i;

        if (lastIndex === undefined)
            return history;

        return history.slice(lastIndex);
    }

    //these 3 methods could be abstracted

    archiveChat = (user: string) => (event: MouseEvent) => {
        event.stopPropagation();

        melan.post('/im/archive', { recipient: user }, _ => {
            this.props.onArchiveClick(user);

            this.setState({ chatOptionsClass: [] });
        });
    }

    markAsRead = (user: string) => (event: MouseEvent) => {
        event.stopPropagation();

        melan.post('/im/read', { recipient: user }, _ => {
            this.props.onMarkAsReadClick(user);

            this.setState({ chatOptionsClass: [] });
        });
    }

    markAsUnread = (user: string) => (event: MouseEvent) => {
        event.stopPropagation();

        melan.post('/im/unread', { recipient: user }, _ => {
            this.props.onMarkAsUnreadClick(user);

            this.setState({ chatOptionsClass: [] });
        });
    }

    matchFilters(contact: UserHistory) {
        return this.state.filter == Filter.All ||
            (this.state.filter == Filter.Active && !contact.archived) ||
            (this.state.filter == Filter.Unread && contact.history.filter(h => isUnread(h, this.props.user)).length > 0);
    }

    filterChats = (filter: Filter) => _ => {
        this.setState({ filterOptionsClass: '', filter });

        history.replaceState({ ...history.state, filter } as BrowserHistoryData, location.pathname);
    }

    setChatting = (userHistory: UserHistory) => {
        this.props.onChattingChange(userHistory);
    }

    showMenu = (field: keyof ContactListState, index?: number) => event => {
        event.stopPropagation();

        this.setState((state, _) => {
            let value = state[field];

            if (field == 'chatOptionsClass')
                value[index] = value[index] ? '' : 'dropdown-wrapper-visible';
            else
                value = value ? '' : 'dropdown-wrapper-visible';

            return { [field]: value };
        });
    }
}

/** Display the current user being chatted or suggested. */
class Profile extends Component<ProfileProps, ProfileState> {

    state = {
        suggestions: this.props.suggestions,
        suggestionIndex: 0
    };

    render() {
        if (this.props.chatting)
            return (
                <div className="suggestion">
                    <a className="skip" title="you need more karma for that">
                        <svg className="i-start svg-50" viewBox="0 0 32 32">
                            <path d="M8 2 L8 16 22 2 22 30 8 16 8 30"></path>
                        </svg>
                    </a>
                    <div className="profile-info">
                        <div>
                            <img className="avatar-profile" src={this.props.chatting.user.avatar} />
                        </div>
                        <h1>{this.props.chatting.user.name}</h1>
                        <h3>{this.props.chatting.user.headline}</h3>
                        <div>{melan.age(this.props.chatting.user.birthday) || ''} {this.props.chatting.user.gender} {melan.countryName(this.props.chatting.user.country)} {melan.languageNames(this.props.chatting.user.languages).join(', ')}</div>
                        <div>1000 karma</div>
                        <div>3 throphies, 5 badges</div>
                        <div>{this.props.chatting.user.tags.join(' ')}</div>
                    </div>
                    <a className="skip green" title="See next profile" onClick={this.nextSuggestion}>
                        <svg className="i-end svg-50" viewBox="0 0 32 32">
                            <path d="M24 2 L24 16 10 2 10 30 24 16 24 30"></path>
                        </svg>
                    </a>
                </div>
            );

        return (<div className="suggestion">
            <img src="/public/media/logo.png" />
        </div>)
    }

    nextSuggestion = () => {
        if (this.state.suggestionIndex == this.state.suggestions.length - 1) {
            //fetch more
        }
        else
            this.setState((state, _) => {
                this.props.onChattingChange(state.suggestions[state.suggestionIndex + 1]);

                return {
                    suggestionIndex: state.suggestionIndex + 1
                };
            });
    }
}

//perhaps we could have a cleaner design by moving contacts to ContactList state and making Chat responsible for sending the messages?
/** Stitches together all the components and handles chat events.  */
class Im extends Component<ImProps, ImState> {

    temporaryID = 0;
    wsProtocol = location.protocol == 'https:' ? 'wss://' : 'ws://';
    socket = new WebSocket(`${this.wsProtocol}${location.host}/im/socket`);
    totalUnread = 0;
    title = document.querySelector('title').innerText;

    state = {
        chatting: this.props.chatting,
        contacts: this.props.contacts
    };

    componentDidMount() {
        this.socket.addEventListener('open', _ => {
            console.log('opening up');
            this.socket.send(JSON.stringify({ token: melan.token() }));
            this.updateReadUnread();
        });

        this.socket.addEventListener('message', event => this.receiveMessage(event));

        this.socket.addEventListener('close', _ => {
            console.log('closed, might want to open it again');
            setTimeout(() => this.socket = new WebSocket(`${this.wsProtocol}${location.host}/im/socket`), 2 * 1000);
        });

        window.addEventListener('popstate', event => {
            let state: BrowserHistoryData = event.state;

            if (state)
                this.setChatting(state.userHistory);
        });
    }

    componentDidUpdate() {
        this.updateReadUnread();
    }

    render() {
        return (
            <>
                <div>
                    <Settings user={this.props.user} />

                    <ContactList user={this.props.user} chatting={this.state.chatting} contacts={this.state.contacts} onChattingChange={this.setChattingBrowserHistory} onArchiveClick={this.archiveChat} onMarkAsReadClick={this.markAsRead} onMarkAsUnreadClick={this.markAsUnread} />

                </div>
                <div className="chat-box">
                    <Profile suggestions={this.props.suggestions} chatting={this.state.chatting} onChattingChange={this.setChattingBrowserHistory} />

                    <Chat user={this.props.user} chatting={this.state.chatting} onMessageSend={this.sendMessage} />
                </div>
            </>
        );
    }

    /** Event for incoming websocket messages. */
    receiveMessage(event: MessageEvent) {
        let response = JSON.parse(event.data);

        console.log(`webscoket message type: ${response.type}`);

        if (response.type == MessageEventStatus.Sent || response.type == MessageEventStatus.Incoming)
            this.updateHistory(response);
    }

    /** Updates state.contacts and state.chatting with incoming or outgoing messages. */
    updateHistory(message: Message) {
        this.setState((state, _) => {
            let chatting = state.chatting ? { ...state.chatting } : undefined,
                contacts = state.contacts.slice(),
                messageUserID = message.recipient || message.sender,
                userHistory = contacts.find(h => h.user.id == messageUserID);

            message.loaded = true;

            //already on contact list
            if (userHistory) {
                //server confirmed a stored a message sent by this user
                if (message.type == MessageEventStatus.Sent) {
                    let previousMessageHistory = userHistory.history.find(h => h.temporaryID == message.temporaryID);
                    
                    previousMessageHistory.id = message.id;
                }
                //incoming message from a different user
                else {
                    userHistory.archived = false;
                    userHistory.history.push(message);
                }
                //so chatting also has the chat history
                if (chatting && userHistory.user.id == chatting.user.id)
                    chatting = userHistory;
            }
            //incoming message from the user being chatted now
            else if (chatting && chatting.user.id == messageUserID) {
                chatting.history.push(message);

                contacts.push(userHistory = chatting);
            }
            else //incoming message from an user which is not on the contact list
                melan.get(`/user/display/${messageUserID}`, {}, user => {
                    this.setState((state, _) => {
                        let contacts = state.contacts.slice();

                        contacts.push({
                            unread: 1,
                            user,
                            history: [message]
                        });

                        return { contacts };
                    });
                });


            return {
                chatting,
                contacts
            };
        });
    }

    updateReadUnread() {
        this.updateReadHistory();
        this.showUnreadCount();
    }

    //THIS HAS TO BE THE SAME AS MARKASREAD!!!11!
    /** Sets messages as read if the current chat has focus. */
    updateReadHistory() {
        if (this.state.chatting && this.state.chatting.history.some(h => isUnread(h, this.props.user)) && document.hasFocus()) {
            melan.post('/im/read', { recipient: this.state.chatting.user.id }, () => {
                this.setState((state, _) => {
                    let chatting = { ...state.chatting },
                        contacts = state.contacts.slice();

                    chatting.history.forEach(m => {
                        if (m.status == MessageStatus.Sent || m.type == MessageEventStatus.Incoming) {
                            m.status = MessageStatus.Read;
                            m.type = MessageEventStatus.Read;
                        }
                    });

                    contacts.find(uh => uh.user.id == chatting.user.id).history = chatting.history;

                    return {
                        chatting,
                        contacts
                    };
                });
            });
        }
    }

    /** Update the window title with how many messages are unread. */
    showUnreadCount() {
        this.totalUnread = this.state.contacts.filter(uh => uh.history.some(h => isUnread(h, this.props.user))).length;

        if (this.totalUnread == 0)
            document.title = `${this.title}`;
        else
            document.title = `(${this.totalUnread}) ${this.title}`;
    }

    archiveChat = (user: string) => {
        this.setState((state, _) => {
            let contacts = state.contacts.slice();

            contacts.find(c => c.user.id == user).archived = true;

            if (state.chatting && state.chatting.user.id == user) {
                this.setBrowserHistory();

                return { contacts, chatting: undefined };
            }
            return { contacts };
        })
    }

    markAsRead = (user: string) => {
        this.markChatAs(MessageStatus.Read, user);
    }

    markAsUnread = (user: string) => {
        this.markChatAs(MessageStatus.Sent, user);

    }

    markChatAs(status: MessageStatus, user: string) {
        this.setState((state, _) => {
            let contacts = state.contacts.slice(),
                chatted = contacts.find(c => c.user.id == user);

            chatted.history.forEach(h => {
                h.status = status;
            });

            if (state.chatting && state.chatting.user.id == user) {
                this.setBrowserHistory();

                return { contacts, chatting: undefined };
            }

            return { contacts };
        });
    }

    /** Sets state.chatting and add to the browser's history if the there was a previous conversation with state.chatting. */
    setChattingBrowserHistory = (userHistory: UserHistory) => {
        this.setChatting(userHistory);

        if (this.state.contacts.some(c => c.user.id == userHistory.user.id))
            this.setBrowserHistory(userHistory);
    }

    /** Sets state.chatting, fetching the chat history from the server. */
    setChatting = (userHistory: UserHistory) => {
        // if (userHistory.history.length > 0)
        this.setState({ chatting: userHistory });
        // else
        //   melan.get('/im/chat/history', { recipient: userHistory.user.id }, history =>
        //     this.setState({ chatting: { ...userHistory, history } }));
    }

    setBrowserHistory(userHistory?: UserHistory) {
        if (userHistory !== undefined) {
            if (history.state == undefined || history.state.userHistory == undefined || history.state.userHistory.user.id != userHistory.user.id)
                history.pushState({ ...history.state, userHistory } as BrowserHistoryData, '', `/im/chat/${userHistory.user.id}`);
        }
        else
            history.pushState(undefined, '', '/im');
    }

    /** Sends a new outgoing message with a temporary key. */
    sendMessage = (content: MessageContent) => {
        let data = {
            id: ++this.temporaryID,
            temporaryID: this.temporaryID,
            content: content.markdown,
            media: content.media,
            token: melan.token(),
            sender: this.props.user.id,
            recipient: this.state.chatting.user.id,
            type: MessageEventStatus.Incoming,
            loaded: content.media === undefined,
            video : content.video
        };

        this.setBrowserHistory(this.state.chatting);

        this.socket.send(JSON.stringify(data));

        this.updateHistory(data);
    }
}

function isUnread(message: Message, user: User) {
    return message.sender != user.id && (message.status == MessageStatus.Sent || message.type == MessageEventStatus.Incoming);
}

declare var initialUser, initialSuggestions, initialContacts, initialChatting, twemoji;

ReactDOM.render(<Im user={initialUser} suggestions={initialSuggestions} contacts={initialContacts} chatting={initialChatting} />, document.querySelector('#im'));

twemoji.parse(document.body);