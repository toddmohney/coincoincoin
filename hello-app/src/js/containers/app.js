import React, { Component } from 'react';
import PropTypes from 'prop-types';
import injectTapEventPlugin from 'react-tap-event-plugin'; // This is required for material-ui
import getMuiTheme from 'material-ui/styles/getMuiTheme';
import {grey800, orange700, white} from 'material-ui/styles/colors';

import AppView from '../Components/App'

injectTapEventPlugin();

class App extends Component {
  render() {
    return (
      <AppView theme={this.muiTheme()}
               children={this.props.children}
      />
    );
  }

  muiTheme() {
    return getMuiTheme({
      palette: {
        primary1Color: orange700,
      },
      appBar: {
        color: grey800
      },
      tabs: {
        backgroundColor: grey800,
        height: 50
      },
      bottomNavigation: {
        backgroundColor: grey800,
        textColor: white
      },
      inkBar: {
        backgroundColor: orange700
      }
    })
  }
}

export default App;
