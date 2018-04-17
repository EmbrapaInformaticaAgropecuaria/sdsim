// Custom message Handlers

// Fix for ace editor for setting empty values to a script area
Shiny.addCustomMessageHandler("shinyAceUpdate", function(data) {
    // Setting variable id value with the same as shinyAce does
    var id = data.id;
    var $el = $("#" + id);
    var editor = $el.data("aceEditor");

    if (data.value === "")
        editor.getSession().setValue("", -1); 
    else 
        editor.getSession().setValue(data.value, -1);

    editor.renderer.updateFull();
});

// Force refresh a ace editor field when it comes into view
Shiny.addCustomMessageHandler("shinyAceForceRefresh", function(id) {
    var $el = $("#" + id);
    var editor = $el.data("aceEditor");
    editor.renderer.updateFull();
});

// Display R error messages in the javascript log
Shiny.addCustomMessageHandler("shinyError", function(value) {
  console.error(value)
});

// Display R warning messages in the javascript log
Shiny.addCustomMessageHandler("shinyWarning", function(value) {
  console.warn(value)
});

// Display R messages in the javascript log
Shiny.addCustomMessageHandler("shinyLog", function(value) {
  console.log(value)
});

// Update simulation progress
Shiny.addCustomMessageHandler("updateSimulationProgress", function(progress) {
  simulationProgress.innerHTML = "Simulation Progress: " + progress + "%";
});

// Display message when a model is loaded
Shiny.addCustomMessageHandler("loadedModelMessage", function(value) {
  if(value !== null) {
    loadedModelMessage.innerText = value[0];
  
    if(value[1] == "green") {
      loadedModelMessage.style.color = "#00873e";
    } else if(value[1] == "red") {
      loadedModelMessage.style.color = "#9e0000";
    }
  }
});

// Display message when a scenario is loaded
Shiny.addCustomMessageHandler("loadedScenarioMessage", function(value) {
  if(value !== null) {
    loadedScenarioMessage.innerText = value[0];
  
    if(value[1] == "green") {
      loadedScenarioMessage.style.color = "#00873e";
    } else if(value[1] == "red") {
      loadedScenarioMessage.style.color = "#9e0000";
    }
  }
});

// Display message when a model ID is changed
Shiny.addCustomMessageHandler("updatedModelIdMessage", function(value) {
  updatedModelIdMessage.innerText = value[0];

  if(value[1] == "green"){
    updatedModelIdMessage.style.color = "#00873e";
  } else if(value[1] == "red") {
    updatedModelIdMessage.style.color = "#9e0000";
  }
});

// Display message when a scenario ID is changed
Shiny.addCustomMessageHandler("updatedScenarioIdMessage", function(value) {
  updatedScenarioIdMessage.innerText = value[0];

  if(value[1] == "green"){
    updatedScenarioIdMessage.style.color = "#00873e";
  } else if(value[1] == "red") {
    updatedScenarioIdMessage.style.color = "#9e0000";
  }
});

// Hide html element
Shiny.addCustomMessageHandler("hideElement", function(value) {
  var panel = document.getElementById(value);
  panel.hidden = true;
});

// Unhide html element
Shiny.addCustomMessageHandler("unhideElement", function(value) {
  var panel = document.getElementById(value);
  panel.hidden = false;
});

// Disable html element
Shiny.addCustomMessageHandler("disableElement", function(value) {
  var panel = document.getElementById(value);
  panel.disabled = true;
});

// Enable html element
Shiny.addCustomMessageHandler("enableElement", function(value) {
  var panel = document.getElementById(value);
  panel.disabled = false;
});

// CTRL S shortcut
$(function() { 
  $(window).keydown(function(e) {
    if(e.ctrlKey && e.which === 83) { // Check for the Ctrl key being pressed, and if the key = [S] (83)
      e.preventDefault();
      Shiny.onInputChange("shortcut", Math.random());
    }
  });
});

// Disable simulation button when clicked (enabled back when simulation is finished)
execSim.onclick = function() {
  execSim.disabled = true;
}

// Temporarily disables simulation button when the selected model or scenario is changed
Shiny.addCustomMessageHandler("delayExecSim", function(value) {
  var panel = document.getElementById("execSim");
  panel.disabled = true;
  setTimeout(function() {panel.disabled = false;}, value);
})
  
// Displays the simulation progress onscreen and re-enables the simulation button
Shiny.addCustomMessageHandler("runningSimulation", function(value) {
  simulationProgress.hidden = !value;
  if(!value) {
    simulationProgress.innerHTML = "Simulation Progress: " + "0" + "%";
    execSim.disabled = false;
  } 
});

// Confirmation before overwriting model or scenario
// Triggers R Shiny event on input with id equal to value.responseInputName
Shiny.addCustomMessageHandler("confirmOverwrite", function(value) {
  var choice = confirm(value.message)
  Shiny.onInputChange(value.responseInputName, [choice, Math.random()]);
})

// Sends alert containing message
Shiny.addCustomMessageHandler("sendAlert", function(message) {
  alert(message);
})

// Prompts user for confirmation before leaving the application
window.onbeforeunload = function confirmExit() {
  return "Do you want to leave this site?\nChanges will not be saved.";
}

// Disables multiple axis button if there are less than 2 variables selected
multipleAxisToggle.disabled = true;
selVarPlot.onchange = function() {
  if(this.length > 1) {
    multipleAxisToggle.disabled = false;
  } else {
    multipleAxisToggle.disabled = true;
  }
}

// Show tooltip when trying to change multiple axis settings with only one variable
multipleAxisTooltipDiv.onclick = function() {
  if(multipleAxisToggle.disabled) {
    multipleAxisTooltipDiv.setAttribute("error-tooltip", "Only available for two or more variables!");
    setTimeout(function() {multipleAxisTooltipDiv.removeAttribute("error-tooltip")}, 2500);
  }
}

// Show tooltip when trying to change method on static models
methodInputDiv.onclick = function() {
  if(method.disabled) {
    methodInputDiv.setAttribute("error-tooltip", "Static models do not use integration method!");
    setTimeout(function() {methodInputDiv.removeAttribute("error-tooltip")}, 2500);
  }
}

// Trigger event when a shiny input is changed
$(document).on("shiny:inputchanged", function(event) {
  var r;
  var panel;
  // Confirmation message before erasing model
  if (event.name === "deleteModel") {
    r = confirm("This will erase your current model.\nAre you sure you want to continue?");
    
    if (r === false) {
      event.preventDefault();
    }
  }
  
  // Confirmation message before erasing scenario
  if (event.name === "deleteScenario") {
    if(selectScenario.value === "Default") {
      alert("The model's default scenario cannot be deleted!")
      event.preventDefault();
    } else {
      r = confirm("This will erase your current scenario.\nAre you sure you want to continue?");
    
      if (r === false) {
        event.preventDefault();
      }
    }
  }
});
