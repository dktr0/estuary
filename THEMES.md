# Estuary's Themes

This document will outline how to customize Estuary's interface themes.

- Estuary allows you to customize its graphical interface via themes (described by CSS). CSS is a programming language that allows you to style a document through modifying its fonts, colors, borders, etc. To create your own Estuary's themes you need to have Estuary installed locally (see BUILDING.md).

- Within estuary's main folder look for the 'static' folder. Inside 'static', access to the 'css-custom' folder.

- Inside 'css-custom' you'll find Estuary's current themes. You can explore all of them to see how they are put together.

- To create a new theme, create a new css file inside of the 'css-custom' folder. To get started, copy the code from one of the current themes in your new css file.

- For quick Estuary's interface customization, modify the parameters of the variables 'background-color', 'primary-color', 'secondary-color', and 'transient-color'. These variables are located at the top of each theme.

```
/* sets the background color */
  --background-color: black;

/* set the color of all text and typically some borders*/
  --primary-color: rgb(98, 221, 115);

/* sets the background color of buttons and other borders */
  --secondary-color: hsl(0,0%,20%);

/* sets the color of things that appear temporarily */
  --transient-color: orange;


```

- For more advanced customization modify the CSS classes below the color variables.

```
/*styles the background */
.background {
    background-color: var(--background-color);
	  }

/*styles the code font*/
.code-font {
	  font-family: 'Helvetica';
	  font-size: 1em;
	  }

/* styles other fonts (e.g. the header, menus and labels) */
.ui-font {
	font-family: 'Helvetica';
	font-size: 1em;
	}

/* styles the user-inteface buttons */
.ui-buttons {
  color:var(--primary-color);
  background-color:var(--secondary-color);
}

/* styles the primary borders (e.g. from dropdown menus)*/
.primary-borders {
	border-radius: 5px;
    border: 1px solid var(--primary-color);
}

/*styles other borders (e.g. from ui buttons and sequencer buttons)  */
.other-borders {
    border-radius: 5px;
    border: 1px solid var(--secondary-color);
    }
```
