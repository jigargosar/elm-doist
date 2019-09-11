# Todoist Clone in Elm with Firebase

## Slice 1: DONE

- firebase auth
- firestore todo CRUD
- Elm Browser.application skeleton with todo CRUD
- Deploy to elmdoist.web.app

## SLICE 2: DONE

- Projects CRUD.
- Project Routes
- InBox
- Deploy to elmdoist.web.app

## 2.5

- [x] cleanup
  - [x] remove now timer.
- [x] todo menu

  - make menu btn larger

## SLICE 3 : DONE

- [x] DueAt
- [x] Today
- [x] Overdue
- [x] Deploy to elmdoist.web.app

## Slice 4: Cleanup existing features.

- [x] fix horizontal scroll issue when there is vertical scrollbar
- [x] fixed top bar sticky position issue, now using fixed.
- [x] add task, signInOut, ... buttons.
- [x] fa checkbox btn
- [x] cleanup dialog ui.
  - Due Dialog
  - Move to Project Dialog.
- [x] Block app when not signed in.
- [x] Deploy to elmdoist.web.app

# Slice 5 : Incomplete Features

- focus input when start edit.
- enter key submits edit form
- toggle sidebar when in small screen.
- show todo item project in "group pending by due date list view".
- home page, describing the project.
- non-empty first use experience.

## Known Issues:

- update today in model.

## Next Slice

- Next Seven Days

## IceBox

- edit text?
- drag/sort todo/project?
- dueAt?
- show completed?
- query/search?

## Maybe

- Firestore Abstraction

## Thinking Elm Api Design

projectList

maybe selected,

type InlineForm =  
 AddForm EditForm None  
viewListWithSelected selI todoList = case selected of None ->

todoList ->

- projectTodoList with either add form at some index or edit form for one
  of the todo's in the list.
- what happens when project todo list changes?
    * change type, ->
        new todo added at index
            -> if we have a list with inline form, then we just need to insert todo at appropriate place.
            -> if we maintain list and form separately:
                then we have to compute and update the new idx of add todo form.
            -> no change required for edit.
        todo with some id deleted
            -> edit form if any, needs to be removed. 
            -> and add form if any, will either auto adjust, or we have to compute new index.            
        todo with some id updated
            -> no change.
            
- do we compute todo list dynamically? every time firebase sends update?
    * consequences:
        - list changes abruptly while user is viewing it.
        - less likely event, since it will occur only when some offline device syncs.
        - since list is dynamic, we have to maintain metadata about any edit/add state.
            - and determine in view how we show the add/edit forms.
        - the add meta depends on the page type: we might have to maintain add 
- or we determine todo list when the route changes. 

- we could use sortIdx and todoId combination to determine in view where to place 
    add edit form.
    with this info, we could show the form at appropriate place.
    one downside is that form will jump, the moment due date is changed.
    
    
- ideally, we should have a list in model, based on current route.
- the list state is determined at load of the route.
- any updated to the list state, either from firestore, or user are then fully controlled.
- but this will lead to us maintaining duplicate data. 
- perhaps we could store a list of ids. and then fetch and sort only those ids.
- then we could indicate addition with a banner on top.
    and deletion with some highlight. and form could remain inline.
    
- with form inlined in the list. how to we get it?
    - using list selection datastructure.
    - i.e. left + maybeform + right
    