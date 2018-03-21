// Editor

require('./editor.sass')
require('./stmt.tag')
require('./expr.tag')
require('./type.tag')
require('./info.tag')

import Mixin from './mixin.js'

import I from 'Block.Bridge'

<editor>
  <nav class="navbar navbar-dark bg-dark">
    <a class="navbar-brand" href="#">Block.hs</a>
  </nav>
  <div class='tab sidebar bg-secondary'>Sidebar</div>
  <div class='tab main bg-light'>
    <main-module data={main_module}/>
  </div>

  <script>
    this.main_module = I.main_module
    this.prelude = I.prelude
    console.log(this.main_module, this.prelude);
  </script>
</editor>

<main-module>
  <statement each={stmt in opts.data} data={stmt}/>

  <script>
  </script>
</main-module>
