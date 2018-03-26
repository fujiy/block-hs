// Sample

import Mixin from './mixin.js'
import I from 'Block.Bridge'

<sample>
  <func-sample data={var}/>

  <script>
    this.mixin(Mixin.Data)

    this.var = I.bindStmtVar(this.data)
  </script>
</sample>

<func-sample class={func: func}>
  <div ref='slot' class='slot'>
    <div class='sample'>
      <div class='term var'>
        <span class='token'>{expr.value0}</span>
      </div>
      <hole each={t, i in holes} data={t} right={i == holes.length - 1} spine='var' conpact={false}/>
    </div>
  </div>

  <type-info show={hover} data={scheme}/>
  <highlight hover={hover}/>

  <script>
    this.mixin(Mixin.Data)
    this.mixin(Mixin.Selectable)
    this.mixin(Mixin.Clonable)

    this.name   = 'expr'
    this.expr   = this.data.value0
    this.scheme = this.data.value1
    this.holes  = I.arrowToArray(this.scheme.value1)
    this.holes.pop()
    this.func   = this.holes.length > 0
  </script>
</func-sample>

// Statement Sample ------------------------------------------------------------

// Expr sample -----------------------------------------------------------------

<expr-sample>
  <div class='slot' ref='slot'>
    <num-expr if={cons == 'num'} data={expr}/>
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
  </script>

</expr-sample>
