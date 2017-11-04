export const FETCH_USERS         = 'FETCH_USERS';
export const FETCH_USERS_SUCCESS = 'FETCH_USERS_SUCCESS';
export const FETCH_USERS_FAILED  = 'FETCH_USERS_FAILED';

export const fetchUsers = (dispatch) => {
  return (apiConfig) => {
    dispatch({
      type: FETCH_USERS,
      payload: { apiConfig }
    });
  }
}
