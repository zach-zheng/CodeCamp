const calorieCounter = document.getElementById('calorie-counter');
const budgetNumberInput = document.getElementById('budget');
const entryDropdown = document.getElementById('entry-dropdown');
const addEntryButton = document.getElementById('add-entry');
const clearButton = document.getElementById('clear');
const output = document.getElementById('output');
let isError = false;

function cleanInputString(str) {
  //console.log("origin string: ", '+-99');
  const regex = /[+-\s]/; // '/' for regular expressions, [] match each of these characters individually
  return str.replace(regex, '');
}
//console.log(cleanInputString('+-99'));

function isInvalidInput(str) {
  //const regex = /e/i; //i flag, which stands for "insensitive".
  //const regex = /[0-9]+e[0-9]+/i; // '+' which mean match one or more digital 
  const regex = /\d+e\d+/i;   // There is a shorthand character class to match any digit: \d.
  return str.match(regex);    //Strings have a .match() method, which takes a regex argument. .match() will return an array of match results.
}

function addEntry() {
  /* 
  const targetId = "#" + entryDropdown.value;
  const targetInputContainer = document.querySelector(`${targetId} .input-container`); // template literal 
  */
  const targetInputContainer = document.querySelector(`#${entryDropdown.value} .input-container`);
  /**To get a count of the number of entries, you can query by text inputs. **/
  const entryNumber = targetInputContainer.querySelectorAll('input[type="text"]').length + 1;
  //dynamic html string 
  const HTMLString = `<label for="${entryDropdown.value}-${entryNumber}-name">Entry ${entryNumber} Name</label><input type="text" placeholder="Name" id="${entryDropdown.value}-${entryNumber}-name">Entry ${entryNumber} Name" /> <label for="${entryDropdown.value}-${entryNumber}-calories">Entry ${entryNumber} Calories</label><input type="number" min="0" placeholder="Calories" id="${entryDropdown.value}-${entryNumber}-calories">Entry ${entryNumber} Calories"/>`;  
  targetInputContainer.insertAdjacentHTML("beforeend", HTMLString);
  
}
function calculateCalories(e) {
  /*The default action of the submit event is to reload the page. You need to prevent this default action using the preventDefault() method of your e parameter. */
  e.preventDefault();
  isError = false;
  const breakfastNumberInputs = document.querySelectorAll("#breakfast input[type='number']");
  const lunchNumberInputs = document.querySelectorAll("#lunch input[type='number']");
  const dinnerNumberInputs = document.querySelectorAll("#dinner input[type='number']");
  const snacksNumberInputs = document.querySelectorAll("#snacks input[type='number']");
  const exerciseNumberInputs = document.querySelectorAll("#exercise input[type='number']");

  const breakfastCalories = getCaloriesFromInputs(breakfastNumberInputs);
  const lunchCalories = getCaloriesFromInputs(lunchNumberInputs);
  const dinnerCalories = getCaloriesFromInputs(dinnerNumberInputs);
  const snacksCalories = getCaloriesFromInputs(snacksNumberInputs);
  const exerciseCalories = getCaloriesFromInputs(exerciseNumberInputs);
}

function getCaloriesFromInputs(list) {
  let calories = 0;
  for (const item of list) {
    const currVal = cleanInputString(item.value);
    const invalidInputMatch = isInvalidInput(currVal);
    if(invalidInputMatch) {
      alert(`Invalid Input: ${invalidInputMatch[0]}`);
      isError = true;
      return null;
    }
    calories += Number(currVal);
  }
  return calories;
}

addEntryButton.addEventListener('click', addEntry);