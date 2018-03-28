// Expr

import Mixin from './mixin.js'
import I from 'Block.Bridge'

// Bind ------------------------------------------------------------------------

<bind>
  <div>
    <bind-left data={data.value0} renew={renewL}/>
    <span class='token'>=</span>
  </div>
  <expr data={data.value1} outer={true}/>

  <script>
    this.mixin(Mixin.Data)
    this.renewL  = d => this.renew(I.renewLeft(d)(this.data))
    this.onrenew = d => this.renew(I.renewRight(d)(this.data))
  </script>
</bind>

<bind-left class='var'>
  <!-- <func-sample data={var}/>
  <div class='term var {args.length == 0 ?"right":""}'>
    <span class='token'>{var.value0.value0}</span>
</div> -->
  <bind-var data={var}/>
  <pattern each={d, i in args} data={d} right={i == args.length - 1} renew={renewA(i)}/>
  <add-area renew={addArg}/>

  <type-info show={hover} data={var.value1}/>
  <highlight outer={outer} hover={hover}/>

  <script>
    this.mixin(Mixin.Data)
    this.args = I.appToArray(this.opts.data)
    this.var  = this.args.shift()

    this.onrenew = d => this.renew(I.toApp(d)(this.args))
    this.renewA  = i => d => this.renew(d ? I.renewArgs(i)(d)(this.args)(this.var)
                                          : I.deleteArg(i)(this.args)(this.var) )
    this.addArg  = d => this.renew(I.toApp(this.var)(this.args.concat(d || I.pempty)))
  </script>
</bind-left>

<add-area onclick={add}>
  <div ref='slot' class='slot'></div>
  <span class='token'>+</span>
  <script>
    this.mixin(Mixin.Data)
    this.mixin(Mixin.Droppable)
    this.add = () => this.renew(null)
  </script>
</add-area>

<bind-var class={func: func}>
  <div ref='slot' class='slot'>
    <div class='sample'>
      <div class='term var'>
        <handle/>
        <input-field data={expr.value0}/>
        <!--<span class='token'>{expr.value0}</span>-->
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

    this.onrenew = s => this.renew(I.renewExpr(I.varC(s))(this.data))
  </script>
</bind-var>

// Expr ------------------------------------------------------------------------

<expr class={left: opts.left, right: opts.right, outer: outer, func: func, bracket: bracket}>
  <div ref='slot' class='slot'>
    <expr-emp if={cons == 'emp'} data={scheme.value1}/>
    <var-expr if={cons == 'var'} data={expr}/>
    <app-expr if={cons == 'app'} data={expr} spine={spine} outer={outer}/>
    <num-expr if={cons == 'num'} data={expr}/>
  </div>
  <hole if={outer && func} spine={spine} each={t, i in holes}
        data={t} right={i == holes.length - 1} renew={apply(i)}/>

  <infos show={hover} data={infos}/>
  <type-info show={hover} data={scheme}/>
  <highlight outer={outer} hover={hover} infos={infos}/>

  <script>
    this.mixin(Mixin.Data)
    this.mixin(Mixin.Selectable)
    this.mixin(Mixin.Draggable)

    this.expr    = this.data.value0
    this.cons    = I.econs(this.expr)
    this.scheme  = this.data.value1
    this.infos   = this.data.value2
    this.holes   = I.arrowToArray(this.scheme.value1); this.holes.pop()
    this.func    = this.holes.length > 0
    this.app     = this.cons == 'app' && !opts.spine
    this.bracket = opts.bracket && (this.func || this.app)
    this.outer   = opts.outer || this.bracket || this.app
    this.spine   = opts.spine || I.econs(I.appToArray(this.data)[0].value0)

    this.name = 'expr'

    this.onrenew = d => this.renew(I.renewExpr(d)(this.data))
    this.apply = i => d => {
        console.log('apply', i, d);
        console.log(I.fillExprWith(i)(d)(this.data));
        this.renew(I.fillExprWith(i)(d)(this.data))
    }

    this.onremove = () => {
        console.log('remove');
        this.renew(I.eempty)
    }
    this.ondrop = d => this.renew(I.assignExpr(this.data)(d))
  </script>
</expr>

<hole class='{opts.spine} {opts.right?"right":""} {conpact?"conpact":""}'>
  <div ref='slot' class='slot'>
    <type data={opts.data}/>
  </div>

  <type-info show={hover} data={scheme}/>
  <highlight hover={hover}/>
  <script>
    this.mixin(Mixin.Data)
    this.mixin(Mixin.Selectable)
    this.mixin(Mixin.Droppable)

    this.name = 'expr'
    this.scheme  = I.spure(this.data)
    this.conpact = opts.conpact == null ? true : opts.conpact

    this.ondrop = d => this.renew(d)
  </script>
</hole>

<handle>
  <div class='grip'></div>
  <button if={opts.remove} onclick={opts.remove} class='remove'>×</button>
</handle>

<highlight class={outer: opts.outer, hover: opts.hover, error: error}>
  <script>
    this.error = opts.infos && opts.infos.errors && opts.infos.errors.length > 0
  </script>
</highlight>

<expr-emp class='term emp'>
  <type data={data}/>
  this.mixin(Mixin.Data)
</expr-emp>

<var-expr class='term var'>
  <span class='token'>{opts.data.value0}</span>
  this.mixin(Mixin.Data)
</var-expr>

<app-expr class='term {opts.spine} {opts.outer?"outer":""}'>
  <expr data={data.value0} spine={opts.spine} left={true} renew={renewL}/>
  <expr data={data.value1} bracket={true} right={opts.outer} renew={renewR}/>
  <script>
    this.mixin(Mixin.Data)
    this.renewL = d => this.renew(I.appC(d)(this.data.value1))
    this.renewR = d => this.renew(I.appC(this.data.value0)(d))
  </script>
</app-expr>

<num-expr class='term num'>
  <span class='token'>{opts.data.value0}</span>
  this.mixin(Mixin.Data)
</num-expr>

// Pattern ---------------------------------------------------------------------

<pattern class={outer: opts.outer, right: opts.right}>
  <div ref='slot' class='slot'>
    <var-pattern if={cons == 'var'} data={expr}/>
  </div>

  <type-info show={hover} data={scheme}/>
  <highlight hover={hover}/>

  <script>
    this.mixin(Mixin.Data)
    this.mixin(Mixin.Selectable)
    this.mixin(Mixin.Clonable)

    this.name   = 'expr'
    this.expr   = this.data.value0
    this.cons   = I.econs(this.expr)
    this.scheme = this.data.value1

    this.onrenew = d => {
        this.renew(d && I.renewExpr(d)(this.data))
    }
  </script>
</pattern>

<var-pattern class='term var'>
  <handle remove={remove}/>
  <input-field data={data.value0}/>
  <!--<span class='token'>{data.value0}</span>-->
  <script>
    this.mixin(Mixin.Data)
    this.onrenew = s => this.renew(I.varC(s))
    this.remove  = () => this.renew(null)
  </script>
</var-pattern>

<input-field>
  <input ref='input' type='text' value={data} onchange={onchange} oninput={oninput}/>
  <script>
    this.mixin(Mixin.Data)
    this.on('mount', () => this.oninput())
    this.onchange = e => this.renew(this.refs.input.value)
    this.oninput  = e => this.refs.input.style.width = this.refs.input.value.length + 'ch'
  </script>
</input-field>
