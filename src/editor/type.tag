// Type

import Mixin from './mixin.js'
import I from 'Block.Bridge'

// Scheme

<scheme>
  <type data={type} outer={true}/>

  <script>
    this.type = opts.data.value1
  </script>
</scheme>

// Type

<type class={outer: outer, left: opts.left, right: opts.right, hole: opts.hole}>
  <type-id      if={cons == 'id'}  data={type}/>
  <type-var     if={cons == 'var'} data={type}/>
  <type-oper    if={cons == 'ope'} data={type}/>
  <type-unknown if={cons == 'unk'} data={type}/>

  <script>
    this.mixin(Mixin.Data)
    this.type = this.data.value0
    this.cons = I.tcons(this.type)
    this.oper    = this.cons == 'ope'
    // this.app     = this.cons == 'app' && !opts.spine
    // this.bracket = (opts.bracket || opts.hole) && this.oper
    this.outer   = opts.outer || this.bracket
  </script>
</type>

<type-id class='term type'>
  <span class='token'>{opts.data.value0}</span>
</type-id>

<type-var class='term type'>
  <span class='token'>{opts.data.value0.value0}</span>
</type-var>

<type-oper class='term opt'>
  <type data={a} hole={true} left={true}/>
  <div class='term opt'>
    <span class='token'>{s}</span>
  </div>
  <type data={b} hole={true} right={true}/>
  <script>
    this.mixin(Mixin.Data)
    this.s = this.data.value0
    this.a = this.data.value1.value0
    this.b = this.data.value2.value0
  </script>
</type-oper>

<type-unknown class='term type'>
  <span class='token'>?</span>
</type-unknown>
