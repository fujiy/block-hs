// Sample

import $ from 'jquery'

import Mixin from './mixin.js'
import I from 'Block.Bridge'

<sample>
  <bind-var data={var}/>

  <script>
    this.mixin(Mixin.Data)

    this.var = I.bindStmtVar(this.data)
  </script>
</sample>

// Statement Sample ------------------------------------------------------------

// Expr sample -----------------------------------------------------------------

<expr-sample>
  <div class='slot' ref='slot'>
    <num-expr    if={cons == 'num'} data={expr}/>
    <expr-lambda if={cons == 'lam'} data={expr}/>
  </div>

  <type-info show={hover} data={scheme}/>
  <highlight outer={outer} hover={hover}/>

  <script>
    this.mixin(Mixin.Data)
    this.mixin(Mixin.Selectable)
    this.mixin(Mixin.Clonable)

    this.name   = 'expr'
    this.expr   = this.data.value0
    this.cons   = I.econs(this.expr)
    this.scheme = this.data.value1

    this.on('mount', () => $(this.root).find('num-expr .token').text('Int'))
  </script>

</expr-sample>
