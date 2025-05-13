import Snabbdom from 'snabbdom-pragma';
import Cmd from './cmd';

import Footer from './footer';
import Navbar from './navbar';

//import router from './router';
//const Router = router.view;

export function view( { state, dispatch } ) {
  return <body class={{"d-flex":true, "flex-column":true, "h-100":true, "min-vh-100": true}}>
    <header>
    <Navbar state={state} dispatch={dispatch} />
    </header>
    <main role="main" class={{"flex-shrink-0":true}} style={{"margin-top": "50px"}}>
    {state.content.children}
    </main>
    <Footer state={state} dispatch={dispatch} />
    </body>;
}

export function update( message, state ) {
  //return router.update( message, state );
  return [ state, Cmd.none ];
}

export function init(content) {
  //return router.init();
  return [{content}, Cmd.none];
}

export default { init, update, view };
