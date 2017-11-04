import { put } from 'redux-saga/effects';
import { browserHistory } from 'react-router';

export function handleApiError(response) {
  return new Promise((resolve, reject) => {
    if (response.ok) {
      resolve(response);
      return;
    }

    response.text().then(function(text){
      reject(new Error(response.statusText + ": " + text));
    })
  })
}
