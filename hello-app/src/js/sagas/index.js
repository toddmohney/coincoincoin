import { fork } from 'redux-saga/effects';

import {
  watchFetchUsers,
} from './users.js'

export default function* startForeman() {
  yield fork(watchFetchUsers);
}
