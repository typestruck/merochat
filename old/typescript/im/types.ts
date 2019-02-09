import { User } from '../types'

export enum MessageEventStatus { Unauthorized, Typing, Sent, Read, Incoming }
export enum Filter { All, Active, Unread }
export enum MessageStatus { Sent = 1, Read }

export type history = Message[];

export interface Message {
    id: number;
    status?: MessageStatus;
    sender?: string;
    recipient?: string;
    content: MessageContent | string;
    type?: MessageEventStatus;
    temporaryID?: number;
    video? : boolean;
    media?: number [];
    loaded? : boolean;
}

export interface MessageContent {
    markdown?: string;
    video? : boolean;
    media?: number [];
}

export interface UserHistory {
    user: User;
    history: history;
    unread: number;
    archived?: boolean;
}

export interface ChatProps {
    user: User;
    chatting: UserHistory;
    onMessageSend: (mc: MessageContent) => void;
}

export interface ChatState {
    messageOnEnter: boolean;
    emojiListVisible: boolean;
    menuClass: string;
    recentEmoji: string[];
}

export interface ImProps {
    user: User;
    suggestions: UserHistory[];
    contacts: UserHistory[];
    chatting: UserHistory;
}

//we could have perhaps a better design in which chatting is an index of contacts?
export interface ImState {
    chatting: UserHistory,
    contacts: UserHistory[]
}

export interface SettingsProps {
    user: User;
}

export interface SettingsState {
    menuClass: string;
}

export interface ContactListProps {
    user: User;
    chatting: UserHistory;
    contacts: UserHistory[];
    onChattingChange: (userHistory: UserHistory) => void
    onArchiveClick: (user: string) => void;
    onMarkAsReadClick: (user: string) => void;
    onMarkAsUnreadClick: (user: string) => void;
}

export interface ContactListState {
    filterOptionsClass: string;
    chatOptionsClass: string[];
    searchQuery: string;
    filter: Filter;
}

export interface ProfileProps {
    suggestions: UserHistory[];
    chatting: UserHistory;
    onChattingChange: (userHistory: UserHistory) => void
}

export interface ProfileState {
    suggestions: UserHistory[];
    suggestionIndex: number;
}

export interface BrowserHistoryData {
    userHistory: UserHistory,
    filter: Filter;
}