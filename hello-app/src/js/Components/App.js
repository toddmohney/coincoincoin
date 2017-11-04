import React, { Component } from 'react';
import PropTypes from 'prop-types';
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';

class App extends Component {
  render() {
    const { children, theme } = this.props;

    return (
      <MuiThemeProvider muiTheme={theme}>
        {children}
      </MuiThemeProvider>
    );
  }
}

App.propTypes = {
  theme: PropTypes.object.isRequired,
}

export default App;
