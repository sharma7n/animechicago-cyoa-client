import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const API_URI = "https://animechicago-cyoa-content.herokuapp.com/game/data";
const DEFAULT_OUTCOME_TYPE = "recommendation";

function initApp(flags) {
  Elm.Main.init({
    node: document.getElementById('root'),
    flags: flags
  });
}

function getOutcomeType(storage) {
  return storage.getItem('outcomeType') || DEFAULT_OUTCOME_TYPE;
}

function saveDataToLocal(data) {
  if (!window.localStorage) {
    console.error("This browser does not support localStorage.");
    return {
      success: false,
      message: "This browser does not support localStorage"
    };
  } else {
    // always save the new data
    window.localStorage.setItem('data', JSON.stringify(data));
    
    // only set config if it does not exist
    var outcomeType = getOutcomeType(window.localStorage);
    window.localStorage.setItem("outcomeType", outcomeType);
    return {
      success: true,
      data: data,
      outcomeType: outcomeType
    };
  }
}

function loadDataFromLocal() {
  if (!window.localStorage) {
    console.error("This browser does not support localStorage.");
    return {
      success: false,
      message: "This browser does not support localStorage"
    };
  } else {
    
    var outcomeType = getOutcomeType(window.localStorage);
    var gameData = JSON.parse(window.localStorage.getItem('data'));
    
    if (gameData) {
      initApp({
        success: true,
        data: gameData,
        outcomeType: outcomeType
      });
    } else {
      console.error("Unable to load data locally.");
    }
  }
}

function loadDataFromNetwork() {
  window.fetch(API_URI)
    .then(response => response.json())
    .then(data => {
      return saveDataToLocal(data);
    })
    .then(flags => {
      initApp(flags);
    })
    .catch(err => {
      console.error("Error loading data from network: " + err);
      loadDataFromLocal();
    });
}

if (window.navigator.onLine) {
  loadDataFromNetwork();
} else {
  loadDataFromLocal();
}

registerServiceWorker();
